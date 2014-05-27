package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.manager.ErmineException

import com.typesafe.config.ConfigFactory

import java.io.File

class ProcessorOutputTest extends UnitSpec {
  val devNull = "file:///dev/null"
  val devNullFile = new File("/dev/null")

  "ProcessorOutput.fromConfigValue" should "build a file output correctly" in {
    val config = ConfigFactory.parseString(s"""io = {name: "a", uri: "${devNull}"}""")
    val output = ProcessorOutput.fromConfigValue(config.getValue("io"))
    output should be (new FileOutput(Some("a"), devNullFile))
  }

  it should "fail when given an unsupported URI" in {
    val config = ConfigFactory.parseString(s"""io = {uri: "mailto:ari@allenai.org"}""")
    an[ErmineException] should be thrownBy {
      ProcessorInput.fromConfigValue(config.getValue("io"))
    }
  }
}
