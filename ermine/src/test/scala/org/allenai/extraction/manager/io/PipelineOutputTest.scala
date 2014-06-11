package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.ErmineException
import org.allenai.extraction.manager.TestErmineModule

import com.typesafe.config.{ ConfigValue, ConfigValueFactory }

import scala.collection.JavaConverters._

import java.io.File

class PipelineOutputTest extends UnitSpec {
  implicit val bindingModule = TestErmineModule

  def configValue(values: (String, AnyRef)*): ConfigValue = {
    ConfigValueFactory.fromMap(values.toMap.asJava)
  }

  "PipelineOutput.fromConfigValue" should "build a file output correctly" in {
    val output = PipelineOutput.fromConfigValue(
      configValue("name" -> "a", "uri" -> "file:///dev/null"))
    output should be(FileOutput(Some("a"), new File("/dev/null")))
  }

  it should "fail when given an unsupported URI" in {
    an[ErmineException] should be thrownBy {
      ProcessorInput.fromConfigValue(configValue("name" -> "a", "uri" -> "mailto:ari@allenai.org"))
    }
  }
}
