package org.allenai.extraction.manager

import com.typesafe.config.{ ConfigObject, ConfigValue }

import java.net.URI
import java.net.URISyntaxException

case class ExtractorIO(name: String, uri: Option[URI])
object ExtractorIO {
  /** Creates a default IO stream using the given ordinal.
    * @param ordinal the index of this default stream in the extractors input, starting at zero
    */
  def defaultIO(ordinal: Int, uri: Option[URI]) = ExtractorIO("$default-" + ordinal, uri)

  /** Parses an IO value from a config value. This can be either an object with optional `name` and
    * `uri` keys, or a raw string. A raw string will be treated as an object with the string as the
    * name.
    * @param ordinal the ordinal to append to any default names created
    */
  def fromConfig(configValue: ConfigValue, ordinal: Int): ExtractorIO = {
    (configValue, configValue.unwrapped) match {
      case (configObject: ConfigObject, _) => {
        val config = configObject.toConfig
        val nameOption = ConfigHelper.getStringOption(config, "name")
        val uri = try {
          ConfigHelper.getStringOption(config, "uri") map { new URI(_) }
        } catch {
          case e: URISyntaxException => throw new ExtractionException("bad uri in config:", e)
        }
        nameOption match {
          case Some(name) => ExtractorIO(name, uri)
          case None => defaultIO(ordinal, uri)
        }
      }
      case (_, name: String) => ExtractorIO(name, None)
      case _ => throw new ExtractionException("expected string or object for IO")
    }
  }
}
