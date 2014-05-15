package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

import java.net.URI

class ProcessorConfigTest extends UnitSpec {
  val validProcessor = "NoOpProcessor"
  val defaultIO = ProcessorIO.defaultIO("0")
  // Binding module for fromConfig calls.
  implicit val bindingModule = TestErmineModule

  // Test that a simple processor works.
  "ProcessorConfig.fromConfig" should "handle a name-only config" in {
    val processorWithInputs = ConfigFactory.parseString(s"""
      name = "${validProcessor}"
      """)
    val processor = ProcessorConfig.fromConfig(processorWithInputs)
    processor.processor should be (NoOpProcessor)
    processor.inputs should be (Seq(defaultIO))
    processor.outputs should be (Seq(defaultIO))
  }

  // Test that we can add inputs & outputs to the pipeline and have them be parsed.
  it should "handle a processor with only inputs configured" in {
    val processorWithInputs = ConfigFactory.parseString(s"""
      name = "${validProcessor}"
      inputs = [ "a" ]
      """)
    val processor = ProcessorConfig.fromConfig(processorWithInputs)
    processor.processor should be (NoOpProcessor)
    processor.inputs should be (Seq(ProcessorIO("a", new URI("name:a"))))
    processor.outputs should be (Seq(defaultIO))
  }
  it should "handle a processor with only outputs configured" in {
    val processorWithInputs = ConfigFactory.parseString(s"""
      name = "${validProcessor}"
      outputs = [ {name: "b"} ]
      """)
    val processor = ProcessorConfig.fromConfig(processorWithInputs)
    processor.processor should be (NoOpProcessor)
    processor.inputs should be (Seq(defaultIO))
    processor.outputs should be (Seq(ProcessorIO("b", new URI("name:b"))))
  }
  it should "handle a processor with both inputs and outputs" in {
    val processorWithInputs = ConfigFactory.parseString(s"""
      name = "${validProcessor}"
      inputs = [ "a" ]
      outputs = [ "x" ]
      """)
    val processor = ProcessorConfig.fromConfig(processorWithInputs)
    processor.processor should be (NoOpProcessor)
    processor.inputs should be (Seq(ProcessorIO("a", new URI("name:a"))))
    processor.outputs should be (Seq(ProcessorIO("x", new URI("name:x"))))
  }

  // Test that we handle bad names gracefully.
  it should "fail gracefully with no name" in {
    val noNameConfig = ConfigFactory.parseString(s"""
    inputs = [ "a" ]
    """)

    an[ErmineException] should be thrownBy {
      ProcessorConfig.fromConfig(noNameConfig)
    }
  }
  it should "fail gracefully with a bad name" in {
    val badNameConfig = ConfigFactory.parseString(s"""
    name = "NotAnProcessor"
    outputs = [ "x", "z" ]
    """)

    an[ErmineException] should be thrownBy {
      ProcessorConfig.fromConfig(badNameConfig)
    }
  }
  it should "fail gracefully with too many inputs" in {
    val badInputs = ConfigFactory.parseString(s"""
    name = "NoOpProcessor"
    // Should only have one input.
    inputs = [ "x", "z" ]
    """)

    an[ErmineException] should be thrownBy {
      ProcessorConfig.fromConfig(badInputs)
    }
  }

  // Test that malformed inputs / outputs are handled gracefully.
  "ProcessorConfig.Builder.getIOValues" should "return the default for an empty array" in {
    val emptyInputsConfig = ConfigFactory.parseString(s"""
    ary = [ ]
    """)

    ProcessorConfig.getIOValues(emptyInputsConfig, "ary", 1) should be (Seq(defaultIO))
  }
}
