package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.ErmineException
import org.allenai.extraction.manager.TestErmineModule

import com.typesafe.config.ConfigValueFactory

import scala.collection.JavaConverters._

import java.io.File

class PipelineInputTest extends UnitSpec {
  implicit val bindingModule = TestErmineModule

  val devNull = "file:///dev/null"

  def configValue(values: (String, AnyRef)*) = {
    ConfigValueFactory.fromMap(values.toMap.asJava)
  }

  "PipelineInput.fromConfigValue" should "build a file input correctly" in {
    val input = PipelineInput.fromConfigValue(configValue("uri" -> "file:///dev/null"))
    input should be(new FileInput(new File("/dev/null")))
  }

  it should "build an unnamed input correctly" in {
    val input = PipelineInput.fromConfigValue(configValue( /* empty */ ))
    input should be(UnnamedInput())
  }

  it should "build a named input correctly" in {
    val input = PipelineInput.fromConfigValue(ConfigValueFactory.fromAnyRef("name"))
    input should be(NamedInput("name"))
  }

  it should "fail when given an unsupported URI" in {
    an[ErmineException] should be thrownBy {
      PipelineInput.fromConfigValue(configValue("uri" -> "mailto:ari@allenai.org"))
    }
  }

  it should "fail when given a name and a URI" in {
    an[ErmineException] should be thrownBy {
      PipelineInput.fromConfigValue(configValue("name" -> "a", "uri" -> devNull))
    }
  }
}
