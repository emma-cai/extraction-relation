package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory

class ErminePipelineTest extends UnitSpec {
  "ErminePipeline.fromConfig" should "parse a simple pipeline correctly" in {
    // Simplest valid pipeline.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)

    pipeline.name should be ("TestWorkflow")
    pipeline.processors.length should be (1)
    pipeline.processors(0).processor should be (NoOpProcessor)
  }
}
