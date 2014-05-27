package org.allenai.extraction.manager.io

import org.allenai.common.Config._
import org.allenai.extraction.manager.ErmineException

import com.typesafe.config.{ ConfigObject, ConfigValue }

import java.net.{ URI, URISyntaxException }

/** Parsed configuration of an input or output to a processor. */
case class IoConfig(name: Option[String], uri: Option[URI])

object IoConfig {
  /** Given a config value, return the IoConfig object that can be built from it. This can be either
    * a ConfigObject, which can have optional `name` and `uri` fields, or a String value. A string
    * will be treated as a URI if it has a ":" in it; else, it will be treated as a name.
    *
    * @throws ErmineException if the given value is neither a string nor a ConfigObject, or if the
    * URI is malformed
    */
  def fromConfigValue(configValue: ConfigValue): IoConfig = {
    (configValue, configValue.unwrapped) match {
      case (configObject: ConfigObject, _) => try {
        val config = configObject.toConfig
        val uri = config.get[String]("uri") map { new URI(_) }
        IoConfig(config.get[String]("name"), uri)
      } catch {
        case e: URISyntaxException =>
          throw new ErmineException("Bad uri in config: " + e.getMessage(), e)
      }
      case (_, string: String) => if (string.contains(':')) {
        IoConfig(None, Some(new URI(string)))
      } else {
        IoConfig(Some(string), None)
      }
      case (_, v) => throw new ErmineException("Expected string or object for IO, got " + v)
    }
  }
}
