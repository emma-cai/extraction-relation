package org.allenai.extraction.manager

import org.allenai.extraction.Extractor
import org.allenai.extraction.stanford._

import com.typesafe.config.{ Config, ConfigException }

import scala.collection.JavaConverters._


/** Configuration for a single extractor. */
case class ExtractorConfig(extractor: Extractor, inputs: Seq[ExtractorIO],
  outputs: Seq[ExtractorIO])
object ExtractorConfig {
  /** Gets the singleton instance of an extractor, or throw a MatchError if we don't know of the
    * extractor.
    */
  def getExtractor(extractorName: String): Extractor = extractorName match {
    case "StanfordParser" => StanfordParser
    case "PrologExtractor" => PrologExtractor
    case "StanfordXmlToTtl" => StanfordXmlToTtl
    case "FerretToExtractionRule" => FerretToExtractionRule
  }

  /** Builds an ExtractorConfig from a config with required key `name` and optional `inputs` and
    * `outputs` keys. If either of `inputs` or `outputs` are missing, they will be set to the value
    * `$default`.
    */
  def fromConfig(config: Config): ExtractorConfig = {
    if (!config.hasPath("name")) {
      throw new ExtractionException(s"extractor missing a 'name' key: ${config}")
    }
    val extractorName = config.getString("name")
    val extractor = try { getExtractor(extractorName) } catch {
      case _: MatchError => throw new ExtractionException(s"unknown extractor '${extractorName}'")
    }

    val inputs = getIOValues(config, "inputs")
    val outputs = getIOValues(config, "outputs")

    if (inputs.size != extractor.numInputs) {
      throw new ExtractionException(s"extrator ${extractorName} requires ${extractor.numInputs} " +
        s"inputs, got ${inputs.size}")
    }
    if (outputs.size != extractor.numOutputs) {
      throw new ExtractionException(s"extrator ${extractorName} requires ${extractor.numOutputs} " +
        s"outputs, got ${outputs.size}")
    }

    ExtractorConfig(extractor, inputs, outputs)
  }

  /** Gets an input or output label set from a config using the given config path. This expects the
    * path to be an array of strings.
    * @throws ExtractionException if the path is not an array of strings
    */
  def getIOValues(config: Config, path: String): Seq[ExtractorIO] = {
    val values: Seq[ExtractorIO] = try {
      // Check for an object at the given path.
      if (config.hasPath(path)) {
        for {
          (configValue, index) <- config.getList(path).asScala.zipWithIndex
        } yield ExtractorIO.fromConfig(configValue, index)
      } else {
        Seq.empty
      }
    } catch {
      case e: ConfigException => {
        throw new ExtractionException(s"bad ${path} value", e)
      }
    }

    if (values.size == 0) {
      // If there were no IO objects configured, return the default (pipe from the previous
      // operation).
      Seq(ExtractorIO.defaultIO(0, None))
    } else {
      values
    }
  }
}
