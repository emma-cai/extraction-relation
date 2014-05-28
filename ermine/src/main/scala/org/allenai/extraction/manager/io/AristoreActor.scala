package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.extraction.manager.ErmineException

import akka.actor._
import org.allenai.ari.datastore.interface.Dataset
import scala.concurrent.Future

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
    case AristoreActor.InitializeOutput(datasetId, directory) => {
      if (datasets.contains(datasetId)) {
        log.info(s"second request to initialize ${datasetId}; ignoring")
      } else {
        log.info(s"initializing ${datasetId}")

        val datasetCache = for {
          dataset <- client.getDataset(datasetId)
        } yield DatasetCache(dataset, directory)

        // TODO(jkinkead): Download files?
        become(receiveWithDatasets(datasets + (datasetId -> datasetCache)))
      }
    }
    case AristoreActor.FinalizeOutput(datasetId) => {
      // TODO(jkinkead): Publish the dataset directory to aristore.
    }
    case unexpected => sender ! Status.Failure(new ErmineException(s"bad message: ${unexpected}"))
  }

  override def receive: Actor.Receive = receiveWithDatasets(Map.empty)
}

/** Actor helping with Aristore IO. */
object AristoreActor {
  case class InitializeOutput(dataset: String, directory: File)

  case class FinalizeOutput(dataset: String)

  // TODO(jkinkead): Figure out how to do this with the akka magic.
  def props(client: AriDatastoreClient): Props = Props(new AristoreActor(client))
}
