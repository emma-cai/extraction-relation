package org.allenai.extraction.manager

import org.allenai.common.Logging
import org.allenai.extraction.Extractor
import org.allenai.extraction.stanford._

import com.typesafe.config.{ Config, ConfigException, ConfigObject, ConfigValue, ConfigValueType }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.Writer
import java.net.URI
import java.net.URISyntaxException

object ConfigHelper {
  /** Gets a string value at a given config path.
    * @throws ExtractionException if the path is missing or isn't a string
    */
  def getString(config: Config, path: String): String = try {
    config.getString(path)
  } catch {
    case e: ConfigException => throw new ExtractionException(s"expected string for key ${path}")
  }

  /** Gets a string value at a given config path, or None if it's missing.
    * @throws ExtractionException if the path is present but not a string
    */
  def getStringOption(config: Config, path: String): Option[String] = if (config.hasPath(path)) {
    val valueType = config.getValue(path).valueType
    if (valueType == ConfigValueType.STRING) {
      Some(config.getString(path))
    } else {
      throw new ExtractionException(s"expected STRING for key ${path}, but was ${valueType}")
     }
  } else {
    None
  }
}

/** Class representing a pipeline. */
class ExtractorPipeline(val name: String, val extractors: Seq[ExtractorConfig]) extends Logging {
  def run(): Unit = {
    runExtractors(extractors, Map.empty)
    // TODO(jkinkead): Sources should probably be returned from runExtractors & closed here.
  }

  /** Recursive function to run extractions. The first extraction in the list will be run, then its
    * outputs will be added to the `sources` map for the next run.
    * @param extractors the extractors to run. Each call runs the first extractor in the Seq.
    * @param sources existing named sources
    */
  @tailrec final def runExtractors(extractors: Seq[ExtractorConfig],
      sources: Map[String, Source]): Unit = extractors match {
    case Seq() => // Base case: No-op.
    case next +: rest => {
      // Get the input(s) that the current extractor stage needs.
      val inputs = next.inputs.map { openSource(_, sources) }
      // Get or create output files for the next extractor to write to.
      val outputFiles = next.outputs.map { getOutputFile }

      // Open writers for the extractors.
      val outputs = outputFiles map { new FileWriter(_) }

      // Run the extractor, closing all IO at the end.
      try {
        next.extractor.extract(inputs, outputs)
      } finally {
        inputs.foreach { _.close }
        outputs.foreach { _.close }
      }

      // Build up the new inputs map from our outputs + the previous one.
      // TODO(jkinkead): This assumes that:
      // *) no names are reused
      // *) no sources are reused
      // *) sources are OK with being left open if unused
      // This should be fixed.
      val outputNames = next.outputs.map { _.name }
      val outputFileSources = outputFiles.map { Source.fromFile(_) }
      val newSources = (outputNames zip outputFileSources).toMap

      runExtractors(rest, sources ++ newSources)
    }
  }

  def openSource(input: ExtractorIO, sources: Map[String, Source]): Source = input match {
    case ExtractorIO(name, None) => sources(name)
    // TODO(jkinkead): URIs with non-default names should be validated (input should still exist).
    // Also, consider making *everything* use a URI.
    case ExtractorIO(_, Some(uri)) => uri.getScheme match {
      case "file" => Source.fromFile(uri)
      case _ => throw new ExtractionException(s"uri ${uri} not supported")
    }
  }

  def getOutputFile(output: ExtractorIO): File = output match {
    case ExtractorIO(name, None) => {
      val outfile = File.createTempFile(name, ".erm")
      outfile.deleteOnExit
      outfile
    }
    case ExtractorIO(_, Some(uri)) => uri.getScheme match {
      case "file" => new File(uri.getPath)
      case _ => throw new ExtractionException(s"uri ${uri} not supported")
    }
  }
}
object ExtractorPipeline {
  /** Builds a pipeline from an extractor config. This expects a Config with keys `name` and
    * `pipeline`, where `name` is a string and `pipeline` is an array of extraction configs.
    */
  def fromConfig(config: Config): ExtractorPipeline = {
    val name = ConfigHelper.getString(config, "name")

    val extractors: Seq[ExtractorConfig] = {
      config.getConfigList("extractors").asScala map { ExtractorConfig.fromConfig }
    }

    if (extractors.length == 0) {
      throw new ExtractionException("no extractors found in pipeline")
    }

    // TODO: Validate the i/o of the extractors.

    new ExtractorPipeline(name, extractors)
  }
}

/** Configuration for an extractor. */
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

case class ExtractorIO(name: String, uri: Option[URI])
object ExtractorIO {
  def defaultIO(ordinal: Int, uri: Option[URI]) = ExtractorIO("$default-" + ordinal, uri)

  /** Parses an IO value from a config value. This can be either an object with optional `name` and
    * `uri` keys, or a raw string. A raw string will be treated as an object with the string as the
    * name.
    * @param ordinal the ordinal to append to any default names created
    */
  def fromConfig(configValue: ConfigValue, ordinal: Int): ExtractorIO = {
    (configValue, configValue.unwrapped) match {
      case (configObject: ConfigObject, _) => {
        val config = configObject.toConfig
        val nameOption = ConfigHelper.getStringOption(config, "name")
        val uri = try {
          ConfigHelper.getStringOption(config, "uri") map { new URI(_) }
        } catch {
          case e: URISyntaxException => throw new ExtractionException("bad uri in config:", e)
        }
        nameOption match {
          case Some(name) => ExtractorIO(name, uri)
          case None => defaultIO(ordinal, uri)
        }
      }
      case (_, name: String) => ExtractorIO(name, None)
      case _ => throw new ExtractionException("expected string or object for IO")
    }
  }
}
