package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

import java.net.URI

class ExtractorConfigTest extends UnitSpec {
  val validExtractor = "NoOpExtractor"
  val defaultIO = ExtractorIO.defaultIO("0")
  val testBuilder = new ExtractorConfig.Builder()(TestErmineModule)

  // Test that a simple extractor works.
  "ExtractorConfig.Builder.fromConfig" should "handle a name-only config" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      """)
    val extractor = testBuilder.fromConfig(extractorWithInputs)
    extractor.extractor should be (NoOpExtractor)
    extractor.inputs should be (Seq(defaultIO))
    extractor.outputs should be (Seq(defaultIO))
  }

  // Test that we can add inputs & outputs to the pipeline and have them be parsed.
  it should "handle an extractor with only inputs configured" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      inputs = [ "a" ]
      """)
    val extractor = testBuilder.fromConfig(extractorWithInputs)
    extractor.extractor should be (NoOpExtractor)
    extractor.inputs should be (Seq(ExtractorIO("a", new URI("name:a"))))
    extractor.outputs should be (Seq(defaultIO))
  }
  it should "handle an extractor with only outputs configured" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      outputs = [ {name: "b"} ]
      """)
    val extractor = testBuilder.fromConfig(extractorWithInputs)
    extractor.extractor should be (NoOpExtractor)
    extractor.inputs should be (Seq(defaultIO))
    extractor.outputs should be (Seq(ExtractorIO("b", new URI("name:b"))))
  }
  it should "handle an extractor with both inputs and outputs" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      inputs = [ "a" ]
      outputs = [ "x" ]
      """)
    val extractor = testBuilder.fromConfig(extractorWithInputs)
    extractor.extractor should be (NoOpExtractor)
    extractor.inputs should be (Seq(ExtractorIO("a", new URI("name:a"))))
    extractor.outputs should be (Seq(ExtractorIO("x", new URI("name:x"))))
  }

  // Test that we handle bad names gracefully.
  it should "fail gracefully with no name" in {
    val noNameConfig = ConfigFactory.parseString(s"""
    inputs = [ "a" ]
    """)

    an[ExtractionException] should be thrownBy {
      testBuilder.fromConfig(noNameConfig)
    }
  }
  it should "fail gracefully with a bad name" in {
    val badNameConfig = ConfigFactory.parseString(s"""
    name = "NotAnExtractor"
    outputs = [ "x", "z" ]
    """)

    an[ExtractionException] should be thrownBy {
      testBuilder.fromConfig(badNameConfig)
    }
  }
  it should "fail gracefully with too many inputs" in {
    val badInputs = ConfigFactory.parseString(s"""
    name = "NoOpExtractor"
    // Should only have one input.
    inputs = [ "x", "z" ]
    """)

    an[ExtractionException] should be thrownBy {
      testBuilder.fromConfig(badInputs)
    }
  }

  // Test that malformed inputs / outputs are handled gracefully.
  "ExtractorConfig.Builder.getIOValues" should "return the default for an empty array" in {
    val emptyInputsConfig = ConfigFactory.parseString(s"""
    ary = [ ]
    """)

    testBuilder.getIOValues(emptyInputsConfig, "ary", 1) should be (Seq(defaultIO))
  }
}
