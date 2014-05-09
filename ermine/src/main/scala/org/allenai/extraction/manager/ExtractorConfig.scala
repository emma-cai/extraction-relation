package org.allenai.extraction.manager

import org.allenai.extraction.Extractor
import org.allenai.extraction.extractors._

import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.{ Config, ConfigException }

import scala.collection.JavaConverters._


/** Configuration for a single extractor. */
case class ExtractorConfig(extractor: Extractor, inputs: Seq[ExtractorIO],
  outputs: Seq[ExtractorIO])
object ExtractorConfig {
  /** Builds an ExtractorConfig from a config with required key `name` and optional `inputs` and
    * `outputs` keys. If either of `inputs` or `outputs` are missing, they will be set to the value
    * `$$default`.
    */
  def fromConfig(config: Config)(implicit bindingModule: BindingModule): ExtractorConfig = {
    val extractors = bindingModule.inject[Map[String,Extractor]](None)

    if (!config.hasPath("name")) {
      throw new ExtractionException(s"extractor missing a 'name' key: ${config}")
    }
    val extractorName = config.getString("name")
    val extractor = extractors.get(extractorName) getOrElse {
      throw new ExtractionException(s"unknown extractor '${extractorName}'")
    }

    val inputs = getIOValues(config, "inputs", extractor.numInputs)
    val outputs = getIOValues(config, "outputs", extractor.numOutputs)

    if (inputs.size != extractor.numInputs) {
      throw new ExtractionException(s"extrator ${extractorName} requires ${extractor.numInputs} " +
        s"inputs, has ${inputs.size} in the configuration")
    }
    if (outputs.size != extractor.numOutputs) {
      throw new ExtractionException(s"extrator ${extractorName} requires ${extractor.numOutputs} " +
        s"outputs, has ${outputs.size} in the configuration")
    }

    ExtractorConfig(extractor, inputs, outputs)
  }

  /** Gets an input or output label set from a config using the given config path. This expects the
    * value at the given path to be an array of strings.
    * @throws ExtractionException if the value at the path is not an array of strings
    */
  def getIOValues(config: Config, path: String, numExpected: Int): Seq[ExtractorIO] = {
    val configuredValues: Seq[ExtractorIO] = try {
      // Check for an object at the given path.
      if (config.hasPath(path)) {
        for {
          (configValue, index) <- config.getList(path).asScala.zipWithIndex
        } yield ExtractorIO.fromConfigValue(configValue, index)
      } else {
        Seq.empty
      }
    } catch {
      case e: ConfigException => {
        throw new ExtractionException(s"bad ${path} value", e)
      }
    }

    if (configuredValues.size == 0) {
      // If there were no IO objects configured, use the defaults (pipe from the previous
      // operation).
      for (i <- 0 until numExpected) yield ExtractorIO.defaultIO(i.toString)
    } else {
      configuredValues
    }
  }
}
