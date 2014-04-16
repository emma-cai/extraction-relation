package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.extractors.PrologExtractor

import com.typesafe.config.ConfigFactory

import java.net.URI

class ExtractorConfigTest extends UnitSpec {
  val validExtractor = "PrologExtractor"
  val defaultIO = ExtractorIO.defaultIO("0")

  // Test that a simple extractor works.
  "ExtractorConfig.fromConfig" should "handle a name-only config" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(defaultIO))
    extractor.outputs should be (Seq(defaultIO))
  }

  // Test that we can add inputs & outputs to the pipeline and have them be parsed.
  it should "handle an extractor with only inputs configured" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      inputs = [ "a" ]
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(ExtractorIO("a", new URI("name:a"))))
    extractor.outputs should be (Seq(defaultIO))
  }
  it should "handle an extractor with only outputs configured" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      outputs = [ {name: "b"} ]
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(defaultIO))
    extractor.outputs should be (Seq(ExtractorIO("b", new URI("name:b"))))
  }
  it should "handle an extractor with both inputs and outputs" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      inputs = [ "a" ]
      outputs = [ "x" ]
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(ExtractorIO("a", new URI("name:a"))))
    extractor.outputs should be (Seq(ExtractorIO("x", new URI("name:x"))))
  }

  // Test that we handle bad names gracefully.
  it should "fail gracefully with no name" in {
    val noNameConfig = ConfigFactory.parseString(s"""
    inputs = [ "a" ]
    """)

    an[ExtractionException] should be thrownBy {
      ExtractorConfig.fromConfig(noNameConfig)
    }
  }
  it should "fail gracefully with a bad name" in {
    val badNameConfig = ConfigFactory.parseString(s"""
    name = "NotAnExtractor"
    outputs = [ "x", "z" ]
    """)

    an[ExtractionException] should be thrownBy {
      ExtractorConfig.fromConfig(badNameConfig)
    }
  }
  it should "fail gracefully with too many inputs" in {
    val badInputs = ConfigFactory.parseString(s"""
    name = "PrologExtractor"
    // Should only have one input.
    inputs = [ "x", "z" ]
    """)

    an[ExtractionException] should be thrownBy {
      ExtractorConfig.fromConfig(badInputs)
    }
  }

  // Test that malformed inputs / outputs are handled gracefully.
  "ExtractorConfig.getIOValues" should "return the default for an empty array" in {
    val emptyInputsConfig = ConfigFactory.parseString(s"""
    ary = [ ]
    """)

    ExtractorConfig.getIOValues(emptyInputsConfig, "ary", 1) should be (Seq(defaultIO))
  }
}
