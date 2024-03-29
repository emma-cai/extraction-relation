package org.allenai.extraction.manager.io

import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.extraction.{ ErmineException, Processor }
import org.allenai.extraction.manager.ErmineModule

import akka.actor.ActorRef
import akka.pattern.ask
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.ConfigValue

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._

import java.io.File
import java.net.URI
import java.nio.file.Files

/** An output from a pipeline. */
sealed abstract class PipelineOutput extends Processor.Output {
  /** @return the name of this output, if one was configured */
  def name: Option[String]

  /** Performs any initialization and validation needed before a pipeline uses this as output, and
    * returns a reference to a fully-initialized output. Some outputs cannot be shared between
    * pipeline executions, and will return new instances from this method.
    * @throws ErmineException if the configured output can't be used
    */
  def initialize()(implicit bindingModule: BindingModule): PipelineOutput = this

  /** Performs any finalization needed before an output is considered finished.  Should be called
    * only after a pipeline completes successfully.
    */
  def commit(): Future[Unit] = Future.successful(Unit)
}
object PipelineOutput {
  /** Builds an output from a config value.
    * @throws ErmineException if the config value has both a name and a URI specified, if the URI
    * scheme is unsupported, or if the config value can't be built into an IoConfig
    */
  def fromConfigValue(configValue: ConfigValue)(
    implicit bindingModule: BindingModule): PipelineOutput = {

    IoConfig.fromConfigValue(configValue) match {
      case IoConfig(name, None) => new EphemeralOutput(name)
      case IoConfig(name, Some(uri)) => buildOutput(name, uri)
    }
  }

  /** Builds the appropriate output from the given URI and name.
    * @throws ErmineException if the URI scheme is unsupported
    */
  def buildOutput(name: Option[String], uri: URI)(
    implicit bindingModule: BindingModule): PipelineOutput = {

    uri.getScheme match {
      case "file" => FileOutput(name, new File(uri))
      case "aristore" => {
        val (datasetId, documentId) = uri.getPath.stripPrefix("/").split("/") match {
          case Array(dataset, document) => (dataset, Some(document))
          case Array(dataset) => (dataset, None)
          case _ => throw new ErmineException(s"Invalid AristorePath: ${uri.getPath}")
        }
        uri.getAuthority match {
          case "file" => new AristoreFileOutputConfig(name, datasetId, documentId)
          case _ =>
            throw new ErmineException(s"Invalid Aristore document type: ${uri.getAuthority}")
        }
      }
      case _ => throw new ErmineException("Unsupported input scheme: " + uri)
    }
  }
}

/** Output that is written to a temp file and discarded after the end of the pipeline run. */
case class EphemeralOutput(override val name: Option[String]) extends PipelineOutput {
  /** Temp file to write output to. Initialized in createTempFile. */
  private var tempFile: File = null

  private def createTempFile(isDirectory: Boolean): File = {
    // Prefix must be of at least length 3, or File.createTempFile will fail.
    val prefix = name match {
      case Some(value) => value + "-erm-"
      case None => "erm-"
    }
    tempFile = if (isDirectory) {
      Files.createTempDirectory(prefix).toFile
    } else {
      File.createTempFile(prefix, ".out")
    }
    tempFile.deleteOnExit()
    tempFile
  }

  override def getOutputFile(): File = Option(tempFile) getOrElse { createTempFile(false) }

  override def getOutputDirectory(): File = {
    if (tempFile == null) {
      createTempFile(true)
    }
    // Call to super to enforce isDirectory.
    super.getOutputDirectory()
  }

  /** Creates a new ephemeral output, which will use a fresh temp file for output. */
  override def initialize()(implicit bindingModule: BindingModule): PipelineOutput = {
    new EphemeralOutput(name)
  }

  /** Deletes the temp file created. */
  override def commit(): Future[Unit] = Future.successful(tempFile.delete())
}

/** Output that is written to a file on disk. */
case class FileOutput(override val name: Option[String], val file: File) extends PipelineOutput {
  override def getOutputFile(): File = file

  /** Validates that the file can be written to. */
  override def initialize()(implicit bindingModule: BindingModule): PipelineOutput = {
    if (!file.exists()) {
      file.createNewFile()
    }
    if (!(file.canWrite)) {
      throw new ErmineException(s"${file.getPath} is not writable or couldn't be created")
    }
    this
  }
}

/** Uninitialized Aristore output. This will throw an exception if getOutputFile is called on it; an
  * initialized instance needs to be fetched.
  * @param datasetId the ID of the dataset to output to
  * @param documentId if set, the single file to output to. If unset, assumes full dataset output,
  * and the file returned will be the directory for dataset output.
  * @see AristoreFileOutput
  */
class AristoreFileOutputConfig(override val name: Option[String], val datasetId: String,
    val documentId: Option[String]) extends PipelineOutput {

  /** Return an AristoreFileOutput that can be used to initiate Aristore output. */
  override def initialize()(implicit bindingModule: BindingModule): PipelineOutput = {
    new AristoreFileOutput(name, datasetId, documentId)
  }

  /** @throws ErmineException when invoked; this represents an uninitialized instance */
  override def getOutputFile(): File = {
    throw new ErmineException("getOutputFile called on uninitialized Aristore output!")
  }
}

/** Initialized Aristore file output.  We need a separate initialized instance to handle writing
  * multiple files in a batch - there's shared per-pipeline state that needs to be kept between all
  * of the Aristore outputs for a given run.  This is kept in AristoreActor.
  * @param documentId if set, the document ID to write to. This is also used as the file name in
  * Aristore. If unset, the directory to write new files to will be returned instead.
  */
class AristoreFileOutput(override val name: Option[String], val datasetId: String,
  val documentId: Option[String])(override implicit val bindingModule: BindingModule)
    extends PipelineOutput with Injectable {

  /** The per-pipeline-execution actor handling Aristore communication and datset batching. */
  val client = inject[ActorRef](AristoreActor.Id)
  val aristoreTimeout = injectOptional[FiniteDuration](AristoreActor.Id) getOrElse { 10.minutes }

  /** The directory we're writing files to. */
  val outputDirectory: Future[File] = {
    val request = AristoreActor.InitializeOutput(datasetId)
    (client ? request)(aristoreTimeout).mapTo[File]
  }

  /** @throws ErmineException when invoked; this copy shouldn't reused between pipeline runs. */
  override def initialize()(implicit bindingModule: BindingModule): PipelineOutput = {
    throw new ErmineException("initialize called on already-initialized AristoreFileOutput!")
  }

  /** @return either a file in the directory for the AristoreActor gave us to write to, named using
    * the provided documentId; or the directory AristoreActor gave us if the documentId is unset
    */
  override def getOutputFile(): File = {
    val directory = Await.result(outputDirectory, aristoreTimeout)
    documentId match {
      case Some(filename) => new File(directory, filename)
      case None => directory
    }
  }

  /** Tells the AristoreActor to commit the given dataset. */
  override def commit(): Future[Unit] = {
    // TODO(jkinkead): Take the timeout from config - we don't want a 10-minute timeout for all
    // executions.
    (client ? AristoreActor.CommitOutput(datasetId))(10.minutes).mapTo[Unit]
  }
}
