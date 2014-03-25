package org.allenai.extraction.manager

import com.typesafe.config.Config

/** Class representing a single workflow. */
case class ExtractionWorkflow(filename: String, extractorUrl: String)
object ExtractionWorkflow {
  def fromConfig(value: Config): ExtractionWorkflow = {
    ExtractionWorkflow(value.getString("input.filename"), value.getString("extractor.url")) 
  }
}
