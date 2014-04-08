package org.allenai.extraction.manager

import org.allenai.common.Logging
import org.allenai.extraction.stanford.PrologExtractor
import org.allenai.extraction.stanford.StanfordParser
import org.allenai.extraction.stanford.StanfordXmlToTtl

import com.typesafe.config.ConfigFactory

import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.StringWriter

/** Main app to run extractions. */
object Ermine extends Logging {
  def main(args: Array[String]): Unit = {
    logger.info("loading pipeline configuration")
    // Load up a config file.
    // TODO(jkinkead): Allow for a custom conf file.
    val config = ConfigFactory.load()
    // TODO(jkinkead): Allow for a custom pipeline config key.
    val configKey = "ermine.pipeline"
    if (!config.hasPath(configKey)) {
      throw new ExtractionException(s"no pipeline configuration found at key ${configKey}")
    }
    val pipeline = ExtractorPipeline.fromConfig(config.getConfig(configKey))

    logger.info("running pipeline")

    pipeline.run
  }
}
