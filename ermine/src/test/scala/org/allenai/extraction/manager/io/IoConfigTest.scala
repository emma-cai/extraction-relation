package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.manager.ErmineException

import com.typesafe.config.ConfigFactory

import java.net.URI

class IoConfigTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullUri = new URI(devNull)

  "IoConfig.fromConfigValue" should "handle a full object correctly" in {
    val config = ConfigFactory.parseString(s"""io = {name: "a", uri: "${devNull}"}""")
    val io = IoConfig.fromConfigValue(config.getValue("io"))
    io should be (IoConfig(Some("a"), Some(devNullUri)))
  }

  it should "handle partial objects correctly" in {
    val noUriConfig = ConfigFactory.parseString(s"""io = {name: "a"}""")
    val noUriIo = IoConfig.fromConfigValue(noUriConfig.getValue("io"))
    noUriIo should be (IoConfig(Some("a"), None))

    val noNameConfig = ConfigFactory.parseString(s"""io = {uri: "${devNull}"}""")
    val noNameIo = IoConfig.fromConfigValue(noNameConfig.getValue("io"))
    noNameIo should be (IoConfig(None, Some(devNullUri)))
  }

  it should "handle bareword names correctly" in {
    val config = ConfigFactory.parseString(s"""io = "barewordName"""")
    val io = IoConfig.fromConfigValue(config.getValue("io"))
    io should be (IoConfig(Some("barewordName"), None))
  }

  it should "handle bareword URIs correctly" in {
    val config = ConfigFactory.parseString(s"""io = "${devNull}"""")
    val io = IoConfig.fromConfigValue(config.getValue("io"))
    io should be (IoConfig(None, Some(devNullUri)))
  }

  it should "fail gracefully with a bad config value" in {
    val config = ConfigFactory.parseString(s"""io = [ { name: "IO is not an array" } ]""")
    an[ErmineException] should be thrownBy {
      IoConfig.fromConfigValue(config.getValue("io"))
    }
  }

  it should "fail gracefully with a bad URI" in {
    val config = ConfigFactory.parseString(s"""io = { uri: "this: is : invalid" }""")
    an[ErmineException] should be thrownBy {
      IoConfig.fromConfigValue(config.getValue("io"))
    }
  }
}
