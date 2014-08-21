package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.ErmineException

import com.typesafe.config.{ ConfigValue, ConfigValueFactory }

import scala.collection.JavaConverters._

import java.net.URI

class IoConfigTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullUri = new URI(devNull)

  def configValue(values: (String, AnyRef)*): ConfigValue = {
    ConfigValueFactory.fromMap(values.toMap.asJava)
  }

  "IoConfig.fromConfigValue" should "handle a full object correctly" in {
    val io = IoConfig.fromConfigValue(configValue("name" -> "a", "uri" -> devNull))
    io should be(IoConfig(Some("a"), Some(devNullUri)))
  }

  it should "handle partial objects correctly" in {
    val noUriIo = IoConfig.fromConfigValue(configValue("name" -> "a"))
    noUriIo should be(IoConfig(Some("a"), None))

    val noNameIo = IoConfig.fromConfigValue(configValue("uri" -> devNull))
    noNameIo should be(IoConfig(None, Some(devNullUri)))
  }

  it should "handle empty objects correctly" in {
    val io = IoConfig.fromConfigValue(configValue( /* empty */ ))
    io should be(IoConfig(None, None))
  }

  it should "handle bareword names correctly" in {
    val io = IoConfig.fromConfigValue(ConfigValueFactory.fromAnyRef("barewordName"))
    io should be(IoConfig(Some("barewordName"), None))
  }

  it should "handle bareword URIs correctly" in {
    val io = IoConfig.fromConfigValue(ConfigValueFactory.fromAnyRef(devNull))
    io should be(IoConfig(None, Some(devNullUri)))
  }

  it should "fail gracefully with a bad config value" in {
    an[ErmineException] should be thrownBy {
      IoConfig.fromConfigValue(ConfigValueFactory.fromAnyRef(1234))
    }
  }

  it should "fail gracefully with a bad URI" in {
    an[ErmineException] should be thrownBy {
      IoConfig.fromConfigValue(configValue("uri" -> "this: is : invalid"))
    }
  }
}
