package org.allenai.extraction.manager

import org.allenai.common.Config._

import com.typesafe.config.{ Config, ConfigException, ConfigObject, ConfigValue }

import java.net.URI
import java.net.URISyntaxException

/** A configured IO stream for a processor. When reading from a configuration file, a user may
  * provide an object with optional `name` and `uri` fields to identify the IO, or they may just
  * provide a bareword name. If no name is given, a special "$$default-N" name will be used.
  *
  * URI schemes will be one of:
  * `file://`, with a path (normal file semantics). This represents a local file to read from or
  * write to.
  * `name:`, with a string value. For output streams, this represents a named stream that can be
  * read from later in the pipeline, but which will not be saved to any location after the pipeline
  * completes. For input streams, this represents a previously-generated named output stream.
  * `default:`, with a numeric value. This is similar to a named stream, with the difference that
  * default outputs are only available to the next processor in the pipeline, and default inputs
  * only read from default outputs of the previous processor in the pipeline.
  */
case class ProcessorIO(name: String, uri: URI) {
  /** @return true if this is a default IO stream */
  def isDefault(): Boolean = uri.getScheme == "default"
}
object ProcessorIO {
  /** Creates a URI with a name and value, aka "scheme-specific part".  As a string, this looks like
    * "scheme:value".
    */
  def simpleUri(scheme: String, value: String) = new URI(scheme, value, null)

  /** Returns the default name for a stream with the given ordinal.
    * @param ordinal the index of this default stream in the processor's input, starting at zero
    */
  def defaultName(ordinal: String) = "$default-" + ordinal
  /** Creates a processor IO with a default name and a default-schemed URI (e.g. "default:0").
    * @param ordinal the index of this default stream in the processor's input, starting at zero
    */
  def defaultIO(ordinal: String) = ProcessorIO(defaultName(ordinal), simpleUri("default", ordinal))

  /** Parses an IO value from a config value. This can be either an object with optional `name` and
    * `uri` keys, or a raw string. A raw string will be treated as an object with the string as the
    * name.
    * @param ordinal the ordinal to append to any default names created
    */
  def fromConfigValue(configValue: ConfigValue, ordinal: Int): ProcessorIO = {
    (configValue, configValue.unwrapped) match {
      case (configObject: ConfigObject, _) => fromConfig(configObject.toConfig, ordinal.toString)
      // Bareword string; create directly.
      case (_, name: String) => ProcessorIO(name, simpleUri("name", name))
      case _ => throw new ErmineException("expected string or object for IO")
    }
  }

  def fromConfig(config: Config, ordinalString: String): ProcessorIO = {
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
      case (None, None) => ProcessorIO.defaultIO(ordinalString)
      // Name but no URI - named only.
      case (Some(name), None) => ProcessorIO(name, simpleUri("name", name))
      // URI only - use default name.
      case (None, Some(uri)) => ProcessorIO(defaultName(ordinalString), uri)
      // Fully configured!
      case (Some(name), Some(uri)) => ProcessorIO(name, uri)
    }
  }
}
