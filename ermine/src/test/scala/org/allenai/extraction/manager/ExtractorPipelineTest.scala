package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

class ExtractorPipelineTest extends UnitSpec {
  "ExtractorPipeline.fromConfig" should "parse a simple pipeline correctly" in {
    // Simplest valid pipeline.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      extractors = [
        {
          name = "NoOpExtractor"
        }
      ]
      """)
    val pipeline = ExtractorPipeline.fromConfig(simpleConfig)(TestErmineModule)

    pipeline.name should be ("TestWorkflow")
    pipeline.extractors.length should be (1)
    pipeline.extractors(0).extractor should be (NoOpExtractor)
  }
}
