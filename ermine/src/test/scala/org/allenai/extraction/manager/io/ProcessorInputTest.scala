package org.allenai.extraction.manager.io

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.manager.ErmineException

import com.typesafe.config.ConfigValueFactory

import scala.collection.JavaConverters._

import java.io.File

class ProcessorInputTest extends UnitSpec {
  val devNull = "file:///dev/null"

  def configValue(values: (String, AnyRef)*) = {
    ConfigValueFactory.fromMap(values.toMap.asJava)
  }

  "ProcessorInput.fromConfigValue" should "build a file input correctly" in {
    val input = ProcessorInput.fromConfigValue(configValue("uri" -> "file:///dev/null"))
    input should be (new FileInput(new File("/dev/null")))
  }

  it should "build an unnamed input correctly" in {
    val input = ProcessorInput.fromConfigValue(configValue(/* empty */))
    input should be (UnnamedInput())
  }

  it should "build a named input correctly" in {
    val input = ProcessorInput.fromConfigValue(ConfigValueFactory.fromAnyRef("name"))
    input should be (NamedInput("name"))
  }

  it should "fail when given an unsupported URI" in {
    an[ErmineException] should be thrownBy {
      ProcessorInput.fromConfigValue(configValue("uri" -> "mailto:ari@allenai.org"))
    }
  }

  it should "fail when given a name and a URI" in {
    an[ErmineException] should be thrownBy {
      ProcessorInput.fromConfigValue(configValue("name" -> "a", "uri" -> devNull))
    }
  }
}
