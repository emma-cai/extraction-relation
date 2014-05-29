package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.ari.datastore.interface.FileDocument
import org.allenai.extraction.manager.ErmineException

import akka.actor.ActorRef
import akka.pattern.ask
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.ConfigValue

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.io.Source

import java.io.File
import java.net.URI
import java.nio.file.Files

/** An input to a pipeline. */
sealed abstract class ProcessorInput {
  /** Performs any initialization and validation needed before a pipeline uses this as input, and
    * returns a reference to a fully-initialized input. Some inputs cannot be shared between
    * pipeline executions, and will return new instances from this method.
    * @throws ErmineException if the configured input can't be used
    */
  def initialize()(implicit bindingModule: BindingModule): ProcessorInput = this

  /** Performs any cleanup of input files needed. */
  def cleanup(): Future[Unit] = Future.successful[Unit](Unit)
}
object ProcessorInput {
  /** Builds an input from a config value.
    * @throws ErmineException if the config value has both a name and a URI specified, if the URI
    * scheme is unsupported, or if the config value can't be built into an IoConfig
    */
  def fromConfigValue(configValue: ConfigValue): ProcessorInput = {

    IoConfig.fromConfigValue(configValue) match {
      case IoConfig(None, None) => UnnamedInput()
      case IoConfig(Some(name), None) => NamedInput(name)
      case IoConfig(None, Some(uri)) => buildUriInput(uri)
      case IoConfig(Some(name), Some(uri)) => throw new ErmineException(
        s"Ambiguous input - both name and URI provided: name=${name}, uri=${uri}")
    }
  }

  /** Builds the appropriate UriInput from the given URI.
    * @throws ErmineException if the URI scheme is unsupported
    */
  def buildUriInput(uri: URI): UriInput = {
    uri.getScheme match {
      case "file" => new FileInput(new File(uri))
      case "aristore" => (uri.getAuthority, uri.getPath.stripPrefix("/").split("/")) match {
        case ("file", Array(datasetId, documentId)) => {
          new AristoreFileInputConfig(datasetId, documentId)
        }
        case _ => throw new ErmineException(s"Invalid Aristore uri: ${uri}")
      }
      case _ => throw new ErmineException("Unsupported input scheme: " + uri)
    }
  }
}

/** An input with just a name. This name is used to look up the processor Source from the available
  * pipeline inputs and previous processor outputs.
  */
case class NamedInput(name: String) extends ProcessorInput

/** An unconfigured input (missing). This will be mapped to the output from the previous stage with
  * the same index (placement in the config list).
  */
case class UnnamedInput() extends ProcessorInput

/** An input with a URI. The input's source will be loaded from the URI. */
sealed abstract case class UriInput() extends ProcessorInput {
  def getSource(): Source
}

/** File input, specified with a file-schemed URI. */
class FileInput(val file: File) extends UriInput() {
  override def getSource(): Source = Source.fromFile(file)
  /** @throws ErmineException if the configured file isn't readable */
  override def initialize()(implicit bindingModule: BindingModule): ProcessorInput = {
    if (!(file.isFile && file.canRead)) {
      throw new ErmineException("${file.getPath} not a file or unreadable")
    }
    this
  }
}

/** Uninitialized Aristore input. This will throw an exception if getSource is called on it; an
  * initialized instance needs to be fetched.
  * @see AristoreFileInput
  */
class AristoreFileInputConfig(val datasetId: String, val documentId: String) extends UriInput() {
  /** @return a new AristoreFileInput with a FileDocument containing the requested file */
  override def initialize()(implicit bindingModule: BindingModule): ProcessorInput = {
    new AristoreFileInput(datasetId, documentId)
  }

  /** @throws ErmineException when invoked; this represents an uninitialized instance */
  override def getSource(): Source = {
    throw new ErmineException("getSource called on uninitialized Aristore input!")
  }
}

/** Initialized Aristore file input.  We need a separate initialized instance to fetch fresh
  * versions of files from Aristore, which we do with a per-pipeline AristoreActor.
  */
class AristoreFileInput(val datasetId: String, val documentId: String)
    (override implicit val bindingModule: BindingModule) extends UriInput() with Injectable {

  /** The per-pipeline-execution actor handling Aristore communication and datset batching. */
  val client = inject[ActorRef](AristoreActor.Id)

  /** A future holding the document we'll be reading. */
  val document: Future[FileDocument] = {
    // Query the datastore for the file requested.
    (client ? AristoreActor.InitializeInput(datasetId, documentId))(10.minutes).mapTo[FileDocument]
  }

  /** @throws ErmineException when invoked; this copy shouldn't reused between pipeline runs. */
  override def initialize()(implicit bindingModule: BindingModule): ProcessorInput = {
    throw new ErmineException("initialize called on already-initialized AristoreFileInput!")
  }

  /** @return the source for the downloaded aristore file */
  override def getSource(): Source = {
    val file = Await.result(document, Duration.Inf)
    Source.fromFile(file.file)
  }

  /** Deletes the file downloaded from Aristore. */
  override def cleanup(): Future[Unit] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    document map { _.file.delete() }
  }
}
