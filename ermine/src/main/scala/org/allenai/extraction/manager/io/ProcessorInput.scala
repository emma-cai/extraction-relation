package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.ari.datastore.interface.FileDocument
import org.allenai.ari.datastore.interface.TextFile
import org.allenai.extraction.manager.ErmineException

import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.ConfigValue

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.io.Source

import java.io.File
import java.net.URI
import java.nio.file.Files

/** An input to a pipeline. */
sealed abstract class ProcessorInput {
  /** Performs any initialization and validation needed before a pipeline uses this as input, and
    * returns a reference to a fully-initialized input.
    * @throws ErmineException if the configured input can't be used
    */
  def initialize(): ProcessorInput = this
}
object ProcessorInput {
  /** Builds an input from a config value.
    * @throws ErmineException if the config value has both a name and a URI specified, if the URI
    * scheme is unsupported, or if the config value can't be built into an IoConfig
    */
  def fromConfigValue(configValue: ConfigValue)
    (implicit bindingModule: BindingModule): ProcessorInput = {

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
  def buildUriInput(uri: URI)(implicit bindingModule: BindingModule): UriInput = {
    uri.getScheme match {
      case "file" => new FileInput(new File(uri))
      case "aristore" => (uri.getAuthority, uri.getPath.stripPrefix("/").split("/")) match {
        case ("file", Array(datasetId, documentId)) => new AristoreFileInput(datasetId, documentId)
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
  override def initialize(): ProcessorInput = {
    if (!(file.isFile && file.canRead)) {
      throw new ErmineException("${file.getPath} not a file or unreadable")
    }
    this
  }
}

/** IO object for Aristore FileDocuments. */
class AristoreFileInput(val datasetId: String, val documentId: String)
    (override implicit val bindingModule: BindingModule) extends UriInput() with Injectable {

  val client: AriDatastoreClient = inject[AriDatastoreClient]

  private lazy val tempDirectory: File = {
    val dir = Files.createTempDirectory(datasetId).toFile
    dir.deleteOnExit
    dir
  }

  private var downloadLocation: Future[FileDocument] = null

  override def initializeInput(): Unit = {
    // TODO(jkinkead): Inject this.
    import scala.concurrent.ExecutionContext.Implicits.global

    // Query the datastore for the file requested!
    downloadLocation = for {
      dataset <- client.getDataset(datasetId)
      document <- client.getFileDocument[TextFile](dataset.id, documentId, tempDirectory)
    } yield document
  }

  override def getSource(): Source = {
    val file = Await.result(downloadLocation, Duration.Inf)
    Source.fromFile(file.file)
  }
}
