package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.api.NoSuchDatasetException
import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.ari.datastore.interface.{ Dataset, DocumentType, FileDocument, TextFile }
import org.allenai.extraction.manager.ErmineException

import akka.actor._
import akka.pattern.pipe
import com.escalatesoft.subcut.inject.{ BindingId, NewBindingModule }
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future
import scala.util.{ Failure, Success }

import java.io.File
import java.nio.file.Files

/** A dataset paired with a location on disk where it's being stored. */
case class DatasetCache(dataset: Dataset, location: File)

class AristoreActor(val client: AriDatastoreClient) extends Actor with ActorLogging {
  import context._

  /** @param datasets the mapping of name to cache value for all of the datasets currently being
    * worked on.
    */
  def receiveWithDatasets(datasets: Map[String, Future[DatasetCache]]): Actor.Receive = {
    case AristoreActor.InitializeInput(datasetId, documentId) => {
      val tempDirectory = Files.createTempDirectory(datasetId).toFile
      tempDirectory.deleteOnExit

      val location: Future[FileDocument] = for {
        dataset <- client.getDataset(datasetId)
        document <- client.getFileDocument[TextFile](dataset.id, documentId, tempDirectory)
      } yield document

      location pipeTo sender
    }
    case AristoreActor.InitializeOutput(datasetId) => {
      if (datasets.contains(datasetId)) {
        log.info(s"already initialized ${datasetId}; returning directory")
        // Send the dataset directory back to the sender.
        (datasets(datasetId) map { _.location}) pipeTo sender
      } else {
        log.info(s"initializing ${datasetId}")

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

        become(receiveWithDatasets(datasets + (datasetId -> datasetCache)))
      }
    }
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
          log.info(s"committed ${datasetId} (${cache})!")
          commit
        }

        // TODO(jkinkead): Fix this.
        commitResult onComplete {
          case Success(_) => log.info("commit done")
          case Failure(x) => log.info(s"commit failed: ${x.getMessage()}")
        }
        commitResult pipeTo sender

        become(receiveWithDatasets(datasets - datasetId))
      } else {
        log.info(s"ignoring commit request for key ${datasetId}; unknown or already committed.")
        sender ! Unit
      }
    }
    case unexpected => sender ! Status.Failure(new ErmineException(s"bad message: ${unexpected}"))
  }

  override def receive: Actor.Receive = receiveWithDatasets(Map.empty)
}

/** Actor helping with Aristore IO. */
object AristoreActor {
  object ActorId extends BindingId

  case class InitializeInput(datasetId: String, documentId: String)

  /** Message directing the actor to start output for a given dataset. */
  case class InitializeOutput(datasetId: String)

  /** Message directing the actor to commit output for a given dataset. */
  case class CommitOutput(datasetId: String)

  // TODO(jkinkead): Figure out how to do this with akka magic.
  def props(client: AriDatastoreClient): Props = Props(new AristoreActor(client))
}

/** Module providing a module-scoped AristoreActor. */
class AristoreActorModule extends NewBindingModule(module => {
  import module._

  bind[ActorRef] idBy AristoreActor.ActorId toModuleSingle { implicit module =>
    val actorSystem = module.inject[ActorSystem](None)
    val aristoreClient = module.inject[AriDatastoreClient](None)
    actorSystem.actorOf(AristoreActor.props(aristoreClient))
  }
})
