package org.allenai.extraction.manager

import org.allenai.common.Config._

import com.typesafe.config.{ Config, ConfigException, ConfigObject, ConfigValue }

import scala.io.Source

import java.io.File
import java.net.URI
import java.net.URISyntaxException

/** A configured IO stream for a processor. When reading from a configuration file, a user may
  * provide an object with optional `name` and `uri` fields to identify the IO, or they may just
  * provide a bareword name. The name configured will be used as the IO object's `key` value. If no
  * name is given, a special "__unnamed-N" name will be used.
  */
sealed abstract class ProcessorIo(val key: String, val isUnnamed: Boolean) {
  /** @return a new temporary file that can be used for IO that will be deleted on JVM exit */
  protected def createTempFile(): File = {
    // Prefix must be of at least length 3, or File.createTempFile will fail.
    val prefix = if (key.length >= 3) {
      key
    } else {
      key + "---"
    }
    val file = File.createTempFile(prefix, ".erm")
    file.deleteOnExit()
    file
  }

  /** @return a Source to read the data currently in this IO */
  def openSource: Source

  /** @return a File to write output data to. This will be re-used to read data from if later
    * processors require it.
    */
  def getOutputFile: File

  /** Performs any initialization and validation needed before a pipeline uses this as input.
    * @throws ErmineException if the IO can't be used as-is for input or output.
    */
  def initializeInput(): Unit = { /* base implementation does nothing */ }

  /** Performs any initialization and validation needed before a pipeline uses this as output.
    * @throws ErmineException if the IO can't be used as-is for input or output.
    */
  def initializeOutput(): Unit = { /* base implementation does nothing */ }

  /** Performs any finalization needed before an output is considered finished.  Should be called
    * only after a pipeline completes successfully.
    */
  def finalizeOutput(): Unit = { /* base implementation does nothing */ }
}

object ProcessorIo {
  /** Creates a URI with a name and value, aka "scheme-specific part".  As a string, this looks like
    * "scheme:value".
    */
  def simpleUri(scheme: String, value: String) = new URI(scheme, value, null)

  /** Returns the string key for an unnamed stream with the given ordinal. Used for the "name" field
    * of the case class.
    * @param ordinal the index of this unnamed stream in the processor's input, starting at zero
    */
  def unnamedKey(ordinal: String) = "__unnamed-" + ordinal
  /** Creates a processor IO with an unnamed-schemed URI (e.g. n unnamed:0").
    * @param ordinal the index of this stream in the processor's input, starting at zero
    */
  def unnamedIO(ordinal: String) = new EphemeralIo(unnamedKey(ordinal), true)

  /** Parses an IO value from a config value. This can be either an object with optional `name` and
    * `uri` keys, or a raw string. A raw string will be treated as an object with the string as the
    * name.
    * @param ordinal the ordinal to use for any unnamed IOs created
    */
  def fromConfigValue(configValue: ConfigValue, ordinal: Int): ProcessorIo = {
    (configValue, configValue.unwrapped) match {
      case (configObject: ConfigObject, _) => fromConfig(configObject.toConfig, ordinal.toString)
      // Bareword string; create directly.
      case (_, name: String) => new EphemeralIo(name, false)
      case _ => throw new ErmineException("expected string or object for IO")
    }
  }

  def fromConfig(config: Config, ordinalString: String): ProcessorIo = {
    val (nameOption, uriOption) = try {
      val name = config.get[String]("name")
      val uri = try {
        config.get[String]("uri") map { new URI(_) }
      } catch {
        case e: URISyntaxException => throw new ErmineException("bad uri in config:", e)
      }
      (name, uri)
    } catch {
      case e: ConfigException => throw new ErmineException("bad config:", e)
    }

    (nameOption, uriOption) match {
      // Default (no configuration).
      case (None, None) => ProcessorIo.unnamedIO(ordinalString)
      // Name but no URI - named only.
      case (Some(name), None) => new EphemeralIo(name, false)
      // URI only - use unnamed name.
      case (None, Some(uri)) => ProcessorIo.fromUri(unnamedKey(ordinalString), true, uri)
      // Fully configured!
      case (Some(name), Some(uri)) => ProcessorIo.fromUri(name, false, uri)
    }
  }

  def fromUri(key: String, isUnnamed: Boolean, uri: URI): ProcessorIo = uri.getScheme match {
    // TODO(jkinkead): Add Aristore support here.
    case "file" => new FileIo(key, isUnnamed, new File(uri.getPath))
  }
}

/** IO object with a user-specified local file. Writes will go to this file and reads will read
  * from it.
  */
case class FileIo(override val key: String, override val isUnnamed: Boolean, val file: File)
    extends ProcessorIo(key, isUnnamed) {
  override def openSource: Source = Source.fromFile(file)
  override def getOutputFile: File = file

  /** Validates that the file can be read from. */
  override def initializeInput(): Unit = {
    if (!(file.isFile && file.canRead)) {
      throw new ErmineException("${file.getPath} not a file or unreadable")
    }
  }

  /** Validates that the file can be written to. */
  override def initializeOutput(): Unit = {
    if (!(file.canWrite)) {
      throw new ErmineException("${file.getPath} not writable")
    }
  }
}

/** IO object without any user-configured destination. This will use a temporary file that is
  * deleted after the pipeline runs.
  */
case class EphemeralIo(override val key: String, override val isUnnamed: Boolean)
    extends ProcessorIo(key, isUnnamed) {
  private val tempFile = createTempFile

  override def openSource: Source = Source.fromFile(tempFile)
  override def getOutputFile: File = tempFile
}
