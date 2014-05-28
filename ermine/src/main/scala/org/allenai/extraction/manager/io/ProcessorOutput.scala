package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.extraction.manager.ErmineException

import akka.actor.ActorSystem
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.ConfigValue

import java.io.File
import java.net.URI

/** An output from a pipeline. */
sealed abstract class ProcessorOutput {
  /** @return the name of this output, if one was configured */
  def name: Option[String]

  /** @return a File to write output data to. This will be re-used to read data from if later
    * processors require it.
    */
  def getOutputFile(): File

  /** Performs any initialization and validation needed before a pipeline uses this as output, and
    * returns a reference to a fully-initialized output.
    * @throws ErmineException if the configured output can't be used
    */
  def initialize(): ProcessorOutput = this

  /** Performs any finalization needed before an output is considered finished.  Should be called
    * only after a pipeline completes successfully.
    */
  def commit(): Unit = { /* base implementation does nothing */ }
}
object ProcessorOutput {
  /** Builds an output from a config value.
    * @throws ErmineException if the config value has both a name and a URI specified, if the URI
    * scheme is unsupported, or if the config value can't be built into an IoConfig
    */
  def fromConfigValue(configValue: ConfigValue)
    (implicit bindingModule: BindingModule): ProcessorOutput = {

    IoConfig.fromConfigValue(configValue) match {
      case IoConfig(name, None) => EphemeralOutput(name)
      case IoConfig(name, Some(uri)) => buildOutput(name, uri)
    }
  }

  /** Builds the appropriate output from the given URI and name.
    * @throws ErmineException if the URI scheme is unsupported
    */
  def buildOutput(name: Option[String], uri: URI)
    (implicit bindingModule: BindingModule): ProcessorOutput = {

    uri.getScheme match {
      case "file" => new FileOutput(name, new File(uri))
      case "aristore" => (uri.getAuthority, uri.getPath.stripPrefix("/").split("/")) match {
        case ("file", Array(datasetId, documentId)) => {
          new AristoreFileOutput(name, datasetId, documentId)
        }
        case _ => throw new ErmineException(s"Invalid Aristore uri: ${uri}")
      }
      case _ => throw new ErmineException("Unsupported input scheme: " + uri)
    }
  }
}

/** Output that is written to a temp file and discarded after the end of the pipeline run. */
case class EphemeralOutput(override val name: Option[String]) extends ProcessorOutput {
  /** Temp file to write output to. Lazy to avoid creating extra empty files until they're needed.
    */
  private lazy val tempFile: File = {
    // Prefix must be of at least length 3, or File.createTempFile will fail.
    val prefix = name match {
      case Some(value) => value + "-erm-"
      case None => "erm-"
    }
    val file = File.createTempFile(prefix, ".out")
    file.deleteOnExit()
    file
  }

  override def getOutputFile(): File = tempFile

  /** Creates a new ephemeral output, which will use a fresh temp file for output. */
  override def initialize(): ProcessorOutput = copy()

  /** Deletes the temp file created. */
  override def commit(): Unit = tempFile.delete()
}

/** Output that is written to a file on disk. */
case class FileOutput(override val name: Option[String], val file: File) extends ProcessorOutput {
  override def getOutputFile(): File = file

  /** Validates that the file can be written to. */
  override def initialize(): ProcessorOutput = {
    if (!file.exists()) {
      file.createNewFile()
    }
    if (!(file.canWrite)) {
      throw new ErmineException(s"${file.getPath} is not writable or couldn't be created")
    }
    this
  }
}

case class AristoreFileOutput(override val name: Option[String], val datasetId: String,
    val documentId: String)(override implicit val bindingModule: BindingModule)
    extends ProcessorOutput with Injectable {

  val aristoreActor = {
    val actorSystem = inject[ActorSystem]
    val aristoreClient = inject[AriDatastoreClient]
    actorSystem.actorOf(AristoreActor.props(aristoreClient))
  }

  override def getOutputFile(): File = null
}
