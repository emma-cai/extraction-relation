package org.allenai.extraction.manager

import org.allenai.common.Config._
import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.processors._

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

import scala.io.Source

import java.io.File
import java.io.StringWriter

class FerretPipelineTest extends UnitSpec {

  "FerretPipeline.fromConfig" should "run a simple pipeline correctly" in {
    // load config
    val configFile = new File("ermine/examples/ferret2.conf")
    val rawConfig = ConfigFactory.parseFile(configFile)
    val resolvedConfig = ConfigFactory.defaultOverrides.withFallback(rawConfig).resolve
    val ferretConfig = resolvedConfig[Config]("ermine.pipeline")

    // build pipeline
    val pipeline = ErminePipeline.fromConfig(ferretConfig)(TestErmineModule)
    pipeline.name should be ("Ferret2")
    pipeline.processors.length should be (1)
    pipeline.processors(0).processor should be (TurtleProcessor)

    // run pipeline
    val output = new StringWriter()
    pipeline.run(Map.empty, Seq.empty, output)
  }

}
