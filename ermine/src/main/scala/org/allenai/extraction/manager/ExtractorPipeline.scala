package org.allenai.extraction.manager

import org.allenai.common.Config._

import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.Config

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.Writer
import java.net.URI

/** Class representing a pipeline. */
class ExtractorPipeline(val name: String, val description: String,
    val extractors: Seq[ExtractorConfig]) {
  /** Run this pipeline, using the given inputs and output.
    * @param inputs the named inputs to this pipeline
    * @param defaultInputs the default (unnamed) inputs to the first stage of this pipeline
    * @param defaultOutput the default output for the last stage of the pipeline
    */
  def run(inputs: Map[String, Source], defaultInputs: Seq[Source], defaultOutput: Writer): Unit = {
    val defaultsMap = (for {
      (input, index) <- defaultInputs.zipWithIndex
    } yield (ExtractorIO.defaultName(index.toString) -> input)).toMap
    runExtractors(extractors, inputs, defaultsMap, defaultOutput)
    inputs.values foreach { _.close }
    defaultInputs foreach { _.close }
    defaultOutput.flush
    defaultOutput.close
  }

  /** Recursive function to run extractions. The first extraction in the list will be run, then its
    * outputs will be added to the `sources` map for the next run.
    * @param extractors the extractors to run. Each call runs the first extractor in the Seq.
    * @param sources existing named sources. All named outputs from the current run will be added to
    *   this map for the next run.
    * @param defaults the default sources for the current extractor run
    */
  @tailrec final def runExtractors(extractors: Seq[ExtractorConfig],
      sources: Map[String, Source], defaults: Map[String, Source], defaultOutput: Writer): Unit = {
    extractors match {
      // Base case: We've run all the extractors; now, if there was a default out from the last
      // step, pipe to the default output.
      case Seq() => {
        for {
          lastOutput <- defaults.get(ExtractorIO.defaultName("0")) if defaults.size == 1
          line <- lastOutput.getLines
        } {
          defaultOutput.write(line)
          defaultOutput.write('\n')
        }
        defaultOutput.flush
      }
      case next +: rest => {
        // Get the input(s) that the current extractor stage needs.
        val inputs = next.inputs map { openSource(_, sources, defaults) }
        // Get or create output files for the next extractor to write to.
        val outputFiles = next.outputs map { getOutputFile }

        // Open writers for the extractors.
        val outputs = outputFiles map { new FileWriter(_) }

        // Run the extractor, closing all IO at the end.
        try {
          next.extractor.extract(inputs, outputs)
        } finally {
          inputs foreach { _.close }
          defaults.values foreach { _.close }
          outputs foreach { output =>
            output.flush
            output.close
          }
        }

        // Build up the new sources for the next round of iteration.
        val (newSources, newDefaults) = (for {
          (output, file) <- next.outputs zip outputFiles
        } yield {
          val pair = Some(output.name -> Source.fromFile(file))
          if (output.isDefault) {
            (None, pair)
          } else {
            (pair, None)
          }
        }).unzip

        runExtractors(rest, sources ++ newSources.flatten, newDefaults.flatten.toMap, defaultOutput)
      }
    }
  }

  def openSource(input: ExtractorIO, sources: Map[String, Source], defaults: Map[String, Source]):
      Source = input.uri.getScheme match {
    case "default" => defaults(input.name)
    case "name" => sources(input.name)
    case "file" => Source.fromFile(input.uri)
    case _ => throw new ExtractionException(s"uri ${input.uri} not supported")
  }

  /** @return an output file for the given ouput IO. This will be a temp file for default or named
    * streams, and the file indicated for a file streams.
    */
  def getOutputFile(output: ExtractorIO): File = output.uri.getScheme match {
    case "default" | "name" => {
      val outfile = File.createTempFile(output.name, ".erm")
      outfile.deleteOnExit
      outfile
    }
    case "file" => new File(output.uri.getPath)
    case _ => throw new ExtractionException(s"uri ${output.uri} not supported")
  }
}
object ExtractorPipeline {
  /** Builds a pipeline from an extractor config. This expects a Config with keys `name` and
    * `pipeline`, where `name` is a string and `pipeline` is an array of extraction configs.
    * The Config may contain a `description` key holding a string.
    */
  def fromConfig(config: Config)(implicit bindingModule: BindingModule): ExtractorPipeline = {
    val name = config.get[String]("name").getOrElse(
      throw new ExtractionException("pipeline name is required"))

    val extractors: Seq[ExtractorConfig] = {
      config.getConfigList("extractors").asScala map { ExtractorConfig.fromConfig }
    }
    val description = config.get[String]("description").getOrElse(name)

    if (extractors.length == 0) {
      throw new ExtractionException("no extractors found in pipeline")
    }

    // TODO: Validate the i/o of the extractors.
    // First step: Validate default (unconfigured) inputs have outputs they can map to.
    // Second step: Determine list of unsatisfied (named) inputs from secondary stages.
    // Third step: Determine required input (required names *OR* required default count).

    new ExtractorPipeline(name, description, extractors)
  }
}
