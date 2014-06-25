package org.allenai.extraction.manager

import org.allenai.common.Config._
import org.allenai.extraction.{ ErmineException, Processor }
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
    val processors: Seq[ProcessorConfig], val requiredNamedInputs: Set[String])(
        implicit val bindingModule: BindingModule) {

  /** The number of unnamed inputs this pipeline requires. */
  val requiredUnnamedCount: Int = if (processors.head.wantsUnnamedInput) {
    processors.head.processor.numInputs
  } else {
    0
  }

  /** Run this pipeline, using the given inputs and output.
    * @param namedSources the named inputs to this pipeline
    * @param unnamedSources the unnamed inputs to the first stage of this pipeline
    * @param defaultOutputs the default outputs for the last stage of the pipeline
    * @throws ErmineException if the provided inputs don't satisfy the pipeline's requirements
    */
  def run(namedSources: Map[String, Source], unnamedSources: Seq[Source],
    defaultOutputs: Seq[Writer]): Unit = {
    // Validate that the inputs match what we need (we aren't missing any).
    val firstProcessor = processors.head
    if (requiredUnnamedCount > unnamedSources.size) {
      throw new ErmineException(s"Pipeline '${name}' requires ${requiredUnnamedCount} unnamed " +
        s"streams; got ${unnamedSources.size}")
    } else {
      val missingInputs = requiredNamedInputs diff namedSources.keySet
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
    val namedInputs = for {
      (name, source) <- namedSources
    } yield (name -> new Processor.SourceInput(source))
    val unnamedInputs = unnamedSources map { new Processor.SourceInput(_) }
    runProcessors(initializedProcessors, namedInputs, unnamedInputs, defaultOutputs)

    // Finalize all inputs & outputs.
    val cleanupFutures: Seq[Future[Unit]] = for {
      processor <- initializedProcessors
      input <- processor.inputs
    } yield input.cleanup()
    val commitFutures: Seq[Future[Unit]] = for {
      processor <- initializedProcessors
      output <- processor.outputs
    } yield output.commit()
    for (f: Future[Unit] <- cleanupFutures ++ commitFutures) Await.result(f, Duration.Inf)

    namedSources.values foreach { _.close }
    unnamedSources foreach { _.close }
    defaultOutputs foreach { output =>
      output.flush
      output.close
    }
  }

  /** Recursive function to run a pipeline. The first processor in the list will be run, then its
    * outputs will be added to the `namedInputs` map for the next run.
    * @param processors the processors to run. Each call runs the first processor in the Seq.
    * @param namedInputs existing named inputs. All named outputs from the current run will be
    * added to this map for the next run.
    * @param unnamedInputs the unnamed inputs for the current processor run. Populated with the
    * outputs of the previous processor run.
    */
  @tailrec final def runProcessors(processors: Seq[ProcessorConfig],
    namedInputs: Map[String, Processor.Input], unnamedInputs: Seq[Processor.Input],
    defaultOutputs: Seq[Writer]): Unit = {

    processors match {
      // Base case: We've run all the processors; now, if there was an unnamed out from the last
      // step, pipe to the default output.
      case Seq() => {
        // TODO(jkinkead): This *always* uses the default output, since the present of unnamed
        // inputs no longer implies no named output. Fix.
        if (unnamedInputs.size <= defaultOutputs.size) {
          for {
            (input, output) <- unnamedInputs zip defaultOutputs
            source <- input.getSources
            line <- source.getLines
          } {
            output.write(line)
            output.write('\n')
          }
          defaultOutputs foreach { _.flush }
        }
      }
      case next +: rest => {
        // Get the input(s) that the current processor stage needs.
        val inputs = next.inputs.zipWithIndex map {
          case (input, index) => {
            input match {
              case UnnamedInput() => unnamedInputs(index)
              case NamedInput(name) => namedInputs(name)
              case uriInput: UriInput => uriInput
            }
          }
        }

        // Run the processor.
        next.processor.process(inputs, next.outputs)

        // Build up the new inputs for the next round of iteration.
        val newUnnamed = for {
          output <- next.outputs
          file = output.getOutputFile
        } yield new FileInput(file)
        val newNamed = for {
          (output, input) <- next.outputs zip newUnnamed
          name <- output.name
        } yield (name -> input)

        runProcessors(rest,
          namedInputs ++ newNamed,
          newUnnamed,
          defaultOutputs)
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
