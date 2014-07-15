package org.allenai.extraction.manager

import org.allenai.common.Config._
import org.allenai.extraction.{ ActorSystemModule, ErmineException }

import akka.actor.ActorSystem
import com.typesafe.config.{ Config, ConfigFactory }
import scopt.OptionParser

import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.PrintWriter
import java.io.Writer

/** Commandline options.
  * @param configFile the (required) config file to load
  * @param pipelineName the pipeline to load from the config file
  * @param input if set, the initial default input to use instead of STDIN
  * @param output if set, the final default output to use instead of STDOUT
  */
case class ErmineOptions(configFile: File = new File("."), pipelineName: String = "ermine.pipeline",
    inputs: Seq[File] = Seq.empty, outputs: Seq[File] = Seq.empty) {
  /** Use the file option(s) as input(s). */
  def defaultInputs: Seq[Source] = inputs map { input =>
    Source.fromFile(input).withDescription(input.getName)
  }

  /** Use the file option as output, default to STDOUT. */
  def defaultOutputs: Seq[Writer] = outputs match {
    case Seq() => Seq(new PrintWriter(System.out))
    case _ => outputs map { new FileWriter(_) }
  }
}

/** Main app to run pipelines. */
object Ermine {
  def main(args: Array[String]): Unit = {
    // format: OFF
    val optionParser = new OptionParser[ErmineOptions]("ermine") {
      opt[File]('c', "config-file") required() valueName("<file>") action { (configFile, options) =>
        options.copy(configFile = configFile)
      } text("The config file to look for a pipeline in")
      opt[String]('p', "pipeline-name") valueName("<name>") action { (pipelineName, options) =>
        options.copy(pipelineName = pipelineName)
      } text("The name of the pipeline to look for in the config file.\n" +
        "        Defaults to ermine.pipeline.")
      opt[File]('i', "input") valueName("<file>") unbounded() action { (input, options) =>
        options.copy(inputs = options.inputs :+ input)
      } text("The input file(s) to send to the first processor.\n" +
        "        Only used if the first processor has no input specified.")
      opt[File]('o', "output") valueName("<file>") unbounded() action { (output, options) =>
        options.copy(outputs = options.outputs :+ output)
      } text("The output file(s) to send to the last pipeline stage.\n" +
        "        Only used if the last processor has no output specified.")
      help("help") text("Prints this help.")
    }
    // format: ON

    try {
      optionParser.parse(args, ErmineOptions()) foreach { options =>
        val actorSystem = ActorSystem("ermine")
        try {
          implicit val module = new ErmineModule(actorSystem) ~ new ActorSystemModule(actorSystem)
          if (!options.configFile.exists()) {
            throw new ErmineException(s"config file ${options.configFile} does not exist")
          }

          // Load up a config file, using the default overrides (system properties).
          val rawConfig = ConfigFactory.parseFile(options.configFile)
          val resolvedConfig = ConfigFactory.defaultOverrides.withFallback(rawConfig).resolve
          val configKey = options.pipelineName
          if (!resolvedConfig.hasPath(configKey)) {
            throw new ErmineException(s"no pipeline configuration found at key ${configKey}")
          }
          val pipeline = ErminePipeline.fromConfig(resolvedConfig[Config](configKey))

          println(s"Running pipeline '${pipeline.name}' . . .")

          val defaultInputs = options.defaultInputs match {
            // If the first pipeline stage wants a single unnamed input, use STDIN.
            case Seq() if pipeline.requiredUnnamedCount == 1 => {
              println("Using input from STDIN (press CTRL-D to end stream)")
              Seq(Source.fromInputStream(System.in).withDescription("stdin"))
            }
            case inputs => inputs
          }

          // TODO(jkinkead): Allow for named pipelines here, too.
          pipeline.run(Map.empty, defaultInputs, options.defaultOutputs)
        } finally {
          actorSystem.shutdown()
        }
      }
    } catch {
      case e: ErmineException =>
        println(s"Error: ${e.getMessage}")
        System.exit(1)
    }
  }
}
