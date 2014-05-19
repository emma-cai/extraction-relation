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

/** Class representing a pipeline.
  * @param name the name of the pipeline, as configured
  * @param description human-readable description of the pipeline
  * @param processors the processors to run in-order
  * @param requiredInputs the named inputs required for this processor to run successfully
  */
class ErminePipeline(val name: String, val description: String,
    val processors: Seq[ProcessorConfig], val requiredInputs: Set[String]) {

  /** The number of default inputs the pipeline requires. */
  val requiredDefaultCount: Int = if (processors.head.wantsDefaultInput) {
    processors.head.processor.numInputs
  } else {
    0
  }

  /** Run this pipeline, using the given inputs and output.
    * @param inputs the named inputs to this pipeline
    * @param defaultInputs the default (unnamed) inputs to the first stage of this pipeline
    * @param defaultOutput the default output for the last stage of the pipeline
    * @throws ErmineException if the provided inputs don't satisfy the pipeline's requirements
    */
  def run(inputs: Map[String, Source], defaultInputs: Seq[Source], defaultOutput: Writer): Unit = {
    // Validate that the inputs match what we need (we aren't missing any).
    val firstProcessor = processors.head
    if (requiredDefaultCount > defaultInputs.size) {
      throw new ErmineException(s"Pipeline '${name}' requires ${requiredDefaultCount} default " +
        s"streams; got ${defaultInputs.size}")
    } else {
      val missingInputs = requiredInputs diff inputs.keySet
      if (missingInputs.size > 0) {
        throw new ErmineException(s"Pipeline '${name}' missing required named inputs. Missing " +
          s"""inputs: ${missingInputs.mkString(", ")}""")
      }
    }
    val defaultsMap = (for {
      (input, index) <- defaultInputs.zipWithIndex
    } yield (ProcessorIO.defaultName(index.toString) -> input)).toMap
    runProcessors(processors, inputs, defaultsMap, defaultOutput)
    inputs.values foreach { _.close }
    defaultInputs foreach { _.close }
    defaultOutput.flush
    defaultOutput.close
  }

  /** Recursive function to run a pipeline. The first processor in the list will be run, then its
    * outputs will be added to the `sources` map for the next run.
    * @param processors the processors to run. Each call runs the first processor in the Seq.
    * @param sources existing named sources. All named outputs from the current run will be added to
    * this map for the next run.
    * @param defaults the default sources for the current processor run
    */
  @tailrec final def runProcessors(processors: Seq[ProcessorConfig],
    sources: Map[String, Source], defaults: Map[String, Source], defaultOutput: Writer): Unit = {
    processors match {
      // Base case: We've run all the processors; now, if there was a default out from the last
      // step, pipe to the default output.
      case Seq() => {
        for {
          lastOutput <- defaults.get(ProcessorIO.defaultName("0")) if defaults.size == 1
          line <- lastOutput.getLines
        } {
          defaultOutput.write(line)
          defaultOutput.write('\n')
        }
        defaultOutput.flush
      }
      case next +: rest => {
        // Get the input(s) that the current processor stage needs.
        val inputs = next.inputs map { openSource(_, sources, defaults) }
        // Get or create output files for the next processor to write to.
        val outputFiles = next.outputs map { getOutputFile }

        // Open writers for the processors.
        val outputs = outputFiles map { new FileWriter(_) }

        // Run the processor, closing all IO at the end.
        try {
          next.processor.process(inputs, outputs)
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

        runProcessors(rest, sources ++ newSources.flatten, newDefaults.flatten.toMap, defaultOutput)
      }
    }
  }

  def openSource(input: ProcessorIO, sources: Map[String, Source],
    defaults: Map[String, Source]): Source = {
    input.uri.getScheme match {
      case "default" => defaults(input.name)
      case "name" => sources(input.name)
      case "file" => Source.fromFile(input.uri)
      case _ => throw new ErmineException(s"uri ${input.uri} not supported")
    }
  }

  /** @return an output file for the given ouput IO. This will be a temp file for default or named
    * streams, and the file indicated for a file streams.
    */
  def getOutputFile(output: ProcessorIO): File = output.uri.getScheme match {
    case "default" | "name" => {
      val outfile = File.createTempFile(output.name, ".erm")
      outfile.deleteOnExit
      outfile
    }
    case "file" => new File(output.uri.getPath)
    case _ => throw new ErmineException(s"uri ${output.uri} not supported")
  }
}
object ErminePipeline {
  /** Builds a pipeline from a processor config. This expects a Config with keys `name` and
    * `pipeline`, where `name` is a string and `pipeline` is an array of processor configs.
    * The Config may contain a `description` key holding a string.
    */
  def fromConfig(config: Config)(implicit bindingModule: BindingModule): ErminePipeline = {
    val name = config.get[String]("name").getOrElse(
      throw new ErmineException("Pipeline name is required"))

    val processors: Seq[ProcessorConfig] = {
      config.getConfigList("processors").asScala map { ProcessorConfig.fromConfig }
    }
    val description = config.get[String]("description").getOrElse(name)

    if (processors.length == 0) {
      throw new ErmineException(s"No processors found in pipeline '${name}'")
    }

    // Validate that each processor using default inputs follows a processor with at least enough
    // outputs to satisfy it.
    for (Seq(previous, current) <- processors.sliding(2)) {
      if (current.wantsDefaultInput) {
        // If this processor expects default input, make sure the previous processor provides
        // enough outputs.
        if (current.inputs.size > previous.outputs.size) {
          throw new ErmineException(s"Processor '${current.name}' has no inputs configured, but " +
            s"previous processor '${previous.name}' only produces ${previous.outputs.size} outputs")
        }
      }
    }

    // Collect all of the unsatisfied named inputs.
    val (_, unsatisfiedInputs) = processors.foldLeft((Set.empty[String], Set.empty[String])) {
      case ((available, unsatisfied), processor) => {
        val newOutputs = (processor.outputs map { _.name }).toSet
        // Gather all named (non-default) inputs that don't have an entry in the available set.
        val newUnsatisfied = (for {
          io <- processor.inputs
          if !io.isDefault
          name = io.name
          if !available.contains(name)
        } yield name).toSet
        (available ++ newOutputs, unsatisfied ++ newUnsatisfied)
      }
    }

    // Validate that either the first pipeline has default inputs OR there are
    // unsatisfied named inputs, but not both.
    // TODO(jkinkead): Expand the commandline API to allow mixing these two ways of specifying
    // input.
    if (processors.head.wantsDefaultInput && unsatisfiedInputs.nonEmpty) {
      throw new ErmineException(s"Pipeline '${name}' requires unnamed inputs, but also requires " +
        s"""named inputs. Required named inputs: ${unsatisfiedInputs.mkString(", ")}""")
    }

    new ErminePipeline(name, description, processors, unsatisfiedInputs)
  }
}
