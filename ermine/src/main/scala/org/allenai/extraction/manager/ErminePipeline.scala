package org.allenai.extraction.manager

import org.allenai.common.Config._
import org.allenai.extraction.manager.io._

import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.Config

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.io.Source

import java.io.{ File, FileWriter, Writer }
import java.net.URI

/** Class representing a pipeline.
  * @param name the name of the pipeline, as configured
  * @param description human-readable description of the pipeline
  * @param processors the processors to run in-order
  * @param requiredNamedInputs the named inputs required for this pipeline to run successfully
  */
class ErminePipeline(val name: String, val description: String,
    val processors: Seq[ProcessorConfig], val requiredNamedInputs: Set[String])
    (implicit val bindingModule: BindingModule) {

  /** The number of unnamed inputs this pipeline requires. */
  val requiredUnnamedCount: Int = if (processors.head.wantsUnnamedInput) {
    processors.head.processor.numInputs
  } else {
    0
  }

  /** Run this pipeline, using the given inputs and output.
    * @param namedInputs the named inputs to this pipeline
    * @param unnamedInputs the unnamed inputs to the first stage of this pipeline
    * @param defaultOutput the default output for the last stage of the pipeline
    * @throws ErmineException if the provided inputs don't satisfy the pipeline's requirements
    */
  def run(namedInputs: Map[String, Source], unnamedInputs: Seq[Source],
    defaultOutput: Writer): Unit = {
    // Validate that the inputs match what we need (we aren't missing any).
    val firstProcessor = processors.head
    if (requiredUnnamedCount > unnamedInputs.size) {
      throw new ErmineException(s"Pipeline '${name}' requires ${requiredUnnamedCount} unnamed " +
        s"streams; got ${unnamedInputs.size}")
    } else {
      val missingInputs = requiredNamedInputs diff namedInputs.keySet
      if (missingInputs.size > 0) {
        throw new ErmineException(s"Pipeline '${name}' missing required named inputs. Missing " +
          s"""inputs: ${missingInputs.mkString(", ")}""")
      }
    }

    // Provide an execution-scoped instance of AristoreActor.
    val pipelineModule = bindingModule ~ new AristoreActorModule()

    // Initialize inputs & outputs.
    val initializedProcessors = for (processor <- processors) yield {
      processor.getInitializedCopy()(pipelineModule)
    }

    // Run.
    runProcessors(initializedProcessors, namedInputs, unnamedInputs, defaultOutput)

    // Finalize all inputs & outputs.
    val cleanupFutures: Seq[Future[Unit]] = for {
      processor <- initializedProcessors
      input <- processor.inputs
    } yield input.cleanup()
    val commitFutures: Seq[Future[Unit]] = for {
      processor <- initializedProcessors
      output <- processor.outputs
    } yield output.commit()
    for (f: Future[Unit] <- cleanupFutures ++ commitFutures) Await.ready(f, Duration.Inf)

    namedInputs.values foreach { _.close }
    unnamedInputs foreach { _.close }
    defaultOutput.flush
    defaultOutput.close
  }

  /** Recursive function to run a pipeline. The first processor in the list will be run, then its
    * outputs will be added to the `namedSources` map for the next run.
    * @param processors the processors to run. Each call runs the first processor in the Seq.
    * @param namedSources existing named sources. All named outputs from the current run will be
    * added to this map for the next run.
    * @param unnamedSources the unnamed sources for the current processor run. Populated with the
    * outputs of the previous processor run.
    */
  @tailrec final def runProcessors(processors: Seq[ProcessorConfig],
    namedSources: Map[String, Source], unnamedSources: Seq[Source],
    defaultOutput: Writer): Unit = {

    processors match {
      // Base case: We've run all the processors; now, if there was an unnamed out from the last
      // step, pipe to the default output.
      case Seq() => {
        if (unnamedSources.size == 1) {
          for {
            line <- unnamedSources(0).getLines
          } {
            defaultOutput.write(line)
            defaultOutput.write('\n')
          }
          defaultOutput.flush
        }
      }
      case next +: rest => {
        // Get the input(s) that the current processor stage needs.
        val inputs = next.inputs.zipWithIndex map { case (input, index) =>
          input match {
            // TODO(jkinkead): The below 'reset' calls are needed in order to be able to reuse
            // inputs to multiple pipeline stages - but they are fragile and should be fixed.
            case UnnamedInput() => unnamedSources(index).reset
            case NamedInput(name) => namedSources(name).reset
            case uriInput: UriInput => uriInput.getSource()
          }
        }

        // Run the processor, closing all IO at the end.
        try {
          next.processor.process(inputs, next.outputs)
        } finally {
          inputs foreach { _.close }
          unnamedSources foreach { _.close }
        }

        // Build up the new sources for the next round of iteration.
        val newUnnamed = for {
          output <- next.outputs
        } yield Source.fromFile(output.getOutputFile)
        val newNamed = for {
          (output, source) <- next.outputs zip newUnnamed
          name <- output.name
        } yield (name -> source)

        runProcessors(rest,
          namedSources ++ newNamed,
          newUnnamed,
          defaultOutput)
      }
    }
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

    // Validate that each processor using unnamed inputs follows a processor with at least enough
    // outputs to satisfy it.
    for (Seq(previous, current) <- processors.sliding(2)) {
      if (current.wantsUnnamedInput) {
        // If this processor expects unnamed input, make sure the previous processor provides
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
        val newOutputs = (processor.outputs map { _.name }).flatten.toSet
        // Gather all named inputs that don't have an entry in the available set.
        val newUnsatisfied = (for {
          NamedInput(name) <- processor.inputs
          if !available.contains(name)
        } yield name).toSet
        (available ++ newOutputs, unsatisfied ++ newUnsatisfied)
      }
    }

    // Validate that either the first pipeline has unnamed inputs OR there are
    // unsatisfied named inputs, but not both.
    // TODO(jkinkead): Expand the commandline API to allow mixing these two ways of specifying
    // input.
    if (processors.head.wantsUnnamedInput && unsatisfiedInputs.nonEmpty) {
      throw new ErmineException(s"Pipeline '${name}' requires unnamed inputs, but also requires " +
        s"""named inputs. Required named inputs: ${unsatisfiedInputs.mkString(", ")}""")
    }

    new ErminePipeline(name, description, processors, unsatisfiedInputs)
  }
}
