package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.api.NoSuchDatasetException
import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.ari.datastore.interface.{ Dataset, DocumentType, FileDocument, TextFile }
import org.allenai.common.Config._
import org.allenai.extraction.{ ConfigModule, ErmineException }

import akka.actor._
import akka.pattern.pipe
import com.escalatesoft.subcut.inject.{ BindingId, NewBindingModule }
import spray.json.DefaultJsonProtocol._
import com.typesafe.config.Config

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

import java.io.File
import java.nio.file.Files

/** An actor for a single pipeline's Aristore operations. This collects all outputs to a single
  * dataset in a single directory, and commits them as a unit.
  */
class AristoreActor(val client: AriDatastoreClient) extends Actor with ActorLogging {
  /** A dataset version paired with the location on disk where it's being stored. */
  case class DatasetCache(dataset: Dataset, location: File)

  import context._

  /** Receives messages defined in the AristoreActor companion object.
    * @param datasets the mapping of name to cache value for all of the datasets currently being
    * worked on.
    */
  def receiveWithDatasets(datasets: Map[String, Future[DatasetCache]]): Actor.Receive = {
    // Reads a file document from the datastore, and returns the location on disk where it's been
    // written to.
    case AristoreActor.InitializeFileInput(datasetId, documentId) => {
      // TODO(jkinkead): Look in the cache first, and cache after reading.
      val tempDirectory = Files.createTempDirectory(datasetId).toFile
      tempDirectory.deleteOnExit

      val location: Future[FileDocument] = for {
        dataset <- client.getDataset(datasetId)
        document <- client.getFileDocument[TextFile](dataset.id, documentId, tempDirectory)
      } yield document

      location pipeTo sender
    }
    // Loads the given dataset from Aristore.
    case AristoreActor.InitializeDatasetInput(datasetId) => {
      // TODO(jkinkead): Look in the cache first, and cache after reading.
      val tempDirectory = Files.createTempDirectory(datasetId).toFile
      tempDirectory.deleteOnExit

      log.debug(s"Downloading full dataset ${datasetId} for read")
      val location: Future[File] = for {
        dataset <- client.getDataset(datasetId)
        _ <- client.readFileDocuments[TextFile](dataset.id, 0, Int.MaxValue, tempDirectory)
      } yield tempDirectory

      location pipeTo sender
    }
    // Looks up the given dataset ID, and starts a create or update operation for it, as
    // appropriate. Returns the directory created to the client.
    case AristoreActor.InitializeOutput(datasetId) => {
      if (datasets.contains(datasetId)) {
        log.debug(s"Already initialized ${datasetId}; returning output directory")
        // Send the dataset directory back to the sender.
        (datasets(datasetId) map { _.location }) pipeTo sender
      } else {
        log.debug(s"Initializing ${datasetId} for write")

        // Look up the dataset, and either create or update.
        val newDataset: Future[Dataset] = client.getDataset(datasetId) flatMap { existingDataset =>
          // Dataset already exists; initiate an update.
          client.updateDataset(existingDataset.id, Seq.empty[String])
        } recoverWith {
          // Exception thrown when the initial lookup fails.
          case _: NoSuchDatasetException =>
            client.createDataset(datasetId, DocumentType.TextFile, Seq.empty[String])
        }
        // Save the updated Dataset object in our cache.
        val datasetCache: Future[DatasetCache] = newDataset map { dataset =>
          val cacheDir = Files.createTempDirectory(datasetId).toFile
          //cacheDir.deleteOnExit
          DatasetCache(dataset, cacheDir)
        }

        // Send the dataset directory back to the sender.
        (datasetCache map { _.location }) pipeTo sender

        // Save the location we set up for the dataset.
        become(receiveWithDatasets(datasets + (datasetId -> datasetCache)))
      }
    }
    // Saves all files that have been added to the given dataset to aristore, then deletes them.
    // Returns a Unit to the sender that it can wait on for commit completion.
    case AristoreActor.CommitOutput(datasetId) => {
      if (datasets.contains(datasetId)) {
        // Publish the dataset directory to aristore; delete the files on disk.
        val commitResult: Future[Unit] = for {
          cache <- datasets(datasetId)
          documents = for (file <- cache.location.listFiles) yield {
            TextFile(file.getName, file.getName, file)
          }
          _ <- client.addDocuments(cache.dataset.id, documents)
          commit <- client.commitDataset(cache.dataset.id)
        } yield {
          // Files have been uploaded and committed; delete them now.
          for (file <- cache.location.listFiles) {
            file.delete()
          }
          log.debug(s"Committed ${datasetId} (${cache})!")
          if (!cache.location.delete()) {
            // Disk leak; should be automatically cleaned up on JVM exit, since the directory is a
            // temp file we told to remove itself.
            log.warning(s"Unable to delete ${cache.location}; leaving around.")
          }
        }

        commitResult pipeTo sender

        become(receiveWithDatasets(datasets - datasetId))
      } else {
        log.debug(s"Ignoring commit request for key ${datasetId}; unknown or already committed.")
        sender ! Unit
      }
    }
    case unexpected => sender ! Status.Failure(new ErmineException(s"bad message: ${unexpected}"))
  }

  /** Starts a receiveWithDatasets  with an empty set of tracked datasets. */
  override def receive: Actor.Receive = receiveWithDatasets(Map.empty)
}

/** Actor helping with Aristore IO. */
object AristoreActor {
  object Id extends BindingId

  /** Initializes a full dataset input, downloading all files in the dataset and returning the
    * directory they are in.
    */
  case class InitializeDatasetInput(datasetId: String)

  /** Initializes input from a single document, returning the file the document was downloaded to.
    */
  case class InitializeFileInput(datasetId: String, documentId: String)

  /** Message directing the actor to start output for a given dataset. */
  case class InitializeOutput(datasetId: String)

  /** Message directing the actor to commit output for a given dataset. */
  case class CommitOutput(datasetId: String)

  def props(client: AriDatastoreClient): Props = Props(new AristoreActor(client))
}

/** Module providing a module-scoped AristoreActor as well as a timeout for talking to Aristore. */
class AristoreActorModule extends NewBindingModule(module => {
  import module._

  val config = ConfigModule.inject[Config](None)

  bind[ActorRef] idBy AristoreActor.Id toModuleSingle { implicit module =>
    val actorSystem = module.inject[ActorSystem](None)
    val aristoreClient = module.inject[AriDatastoreClient](None)
    actorSystem.actorOf(AristoreActor.props(aristoreClient))
  }

  // Bind the ari datastore timeout, if it's configured.
  config.get[Long]("aristore.timeoutMillis") foreach { timeout =>
    bind[FiniteDuration] idBy AristoreActor.Id toSingle timeout.millis
  }
})
