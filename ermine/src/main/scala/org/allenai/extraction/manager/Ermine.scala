package org.allenai.extraction.manager

import org.allenai.common.Logging

import com.typesafe.config.ConfigFactory
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
  inputs: Seq[File] = Seq.empty, output: Option[File] = None) {
  /** Use the file option(s) as input(s), default to STDIN. */
  def defaultInputs: Seq[Source] = inputs match {
    case Seq() => Seq(Source.fromInputStream(System.in))
    case _ => inputs map Source.fromFile
  }
  /** Use the file option as output, default to STDOUT. */
  def defaultOutput: Writer = output map {
    new FileWriter(_)
  } getOrElse {
    new PrintWriter(System.out)
  }
}

/** Main app to run extractions. */
object Ermine extends Logging {
  def main(args: Array[String]): Unit = {
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
      } text("The default input file to send to the first extractor.\n" +
        "        Only used if the first extractor has no input specified.")
      opt[File]('o', "output") valueName("<file>") action { (output, options) =>
        options.copy(output = Some(output))
      } text("The default output file to send to the first pipeline stage.\n" +
        "        Only used if the first extractor has no output specified.")
      help("help") text("Prints this help.")
    }

    optionParser.parse(args, ErmineOptions()) foreach { options =>
      logger.info("loading pipeline configuration")

      // Load up a config file.
      val config = ConfigFactory.parseFile(options.configFile)
      val configKey = options.pipelineName
      if (!config.hasPath(configKey)) {
        throw new ExtractionException(s"no pipeline configuration found at key ${configKey}")
      }
      val pipeline = ExtractorPipeline.fromConfig(config.getConfig(configKey))

      logger.info("running pipeline")

      // TODO(jkinkead): Allow for named pipelines here, too.
      pipeline.run(Map.empty, options.defaultInputs, options.defaultOutput)
    }
  }
}
