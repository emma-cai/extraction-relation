package org.allenai.extraction.manager

import org.allenai.common.Logging

import com.typesafe.config.{ Config, ConfigException, ConfigValueType }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.Writer

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
  def getStringOption(config: Config, path: String): Option[String] = {
    if (config.hasPath(path)) {
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
