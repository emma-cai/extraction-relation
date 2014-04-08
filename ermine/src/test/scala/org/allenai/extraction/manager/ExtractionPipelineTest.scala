package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.stanford.PrologExtractor

import com.typesafe.config.ConfigFactory

class ExtractionWorkflowTest extends UnitSpec {
  val validExtractor = "PrologExtractor"
  val defaultIO = ExtractorIO.defaultIO(0, None)

  // Test that a simple extractor works.
  "A name-only extractor" should "parse correctly" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(defaultIO))
    extractor.outputs should be (Seq(defaultIO))
  }

  // Test that we can add inputs & outputs to the pipeline and have them be parsed.
  "An extractor with inputs" should "parse correctly" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      inputs = [ "a" ]
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(ExtractorIO("a", None)))
    extractor.outputs should be (Seq(defaultIO))
  }
  "An extractor with outputs" should "parse correctly" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      outputs = [ {name: "b"} ]
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(defaultIO))
    extractor.outputs should be (Seq(ExtractorIO("b", None)))
  }
  "An extractor with inputs and outputs" should "parse correctly" in {
    val extractorWithInputs = ConfigFactory.parseString(s"""
      name = "${validExtractor}"
      inputs = [ "a" ]
      outputs = [ "x" ]
      """)
    val extractor = ExtractorConfig.fromConfig(extractorWithInputs)
    extractor.extractor should be (PrologExtractor)
    extractor.inputs should be (Seq(ExtractorIO("a", None)))
    extractor.outputs should be (Seq(ExtractorIO("x", None)))
  }

  // Test that we handle bad names gracefully.
  "An extractor with no name" should "fail gracefully" in {
    val noNameConfig = ConfigFactory.parseString(s"""
    inputs = [ "a" ]
    """)

    an[ExtractionException] should be thrownBy {
      ExtractorConfig.fromConfig(noNameConfig)
    }
  }
  "An extractor with a bad name" should "fail gracefully" in {
    val badNameConfig = ConfigFactory.parseString(s"""
    name = "NotAnExtractor"
    outputs = [ "x", "z" ]
    """)

    an[ExtractionException] should be thrownBy {
      ExtractorConfig.fromConfig(badNameConfig)
    }
  }
  "An extractor with a bad inputs" should "fail gracefully" in {
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
  "An empty IO  array" should "return defaults" in {
    val emptyInputsConfig = ConfigFactory.parseString(s"""
    ary = [ ]
    """)

    ExtractorConfig.getIOValues(emptyInputsConfig, "ary") should be (Seq(defaultIO))
  }

  "A simple pipeline" should "parse correctly" in {
    // Simplest valid pipeline.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      extractors = [
        {
          name = "${validExtractor}"
        }
      ]
      """)
    val pipeline = ExtractorPipeline.fromConfig(simpleConfig)

    pipeline.name should be ("TestWorkflow")
    pipeline.extractors.length should be (1)
    pipeline.extractors(0).extractor should be (PrologExtractor)
  }
}
