package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.manager.ErmineException

import com.typesafe.config.ConfigFactory

import java.io.File

class ProcessorInputTest extends UnitSpec {
  val devNull = "file:///dev/null"

  "ProcessorInput.fromConfigValue" should "build a file input correctly" in {
    val config = ConfigFactory.parseString(s"""io = {uri: "${devNull}"}""")
    val input = ProcessorInput.fromConfigValue(config.getValue("io"))
    input should be (new FileInput(new File(devNull)))
  }

  it should "build an unnamed input correctly" in {
    val config = ConfigFactory.parseString(s"""io = {}""")
    val input = ProcessorInput.fromConfigValue(config.getValue("io"))
    input should be (UnnamedInput())
  }

  it should "build a named input correctly" in {
    val config = ConfigFactory.parseString(s"""io = "name"""")
    val input = ProcessorInput.fromConfigValue(config.getValue("io"))
    input should be (NamedInput("name"))
  }

  it should "fail when given an unsupported URI" in {
    val config = ConfigFactory.parseString(s"""io = {uri: "mailto:ari@allenai.org"}""")
    an[ErmineException] should be thrownBy {
      ProcessorInput.fromConfigValue(config.getValue("io"))
    }
  }

  it should "fail when given a name and a URI" in {
    val config = ConfigFactory.parseString(s"""io = { name: "a", uri: "${devNull}"}""")
    an[ErmineException] should be thrownBy {
      ProcessorInput.fromConfigValue(config.getValue("io"))
    }
  }
}
