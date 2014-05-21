package org.allenai.extraction.manager

import org.allenai.extraction.Processor
import org.allenai.extraction.processors._

import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.{ Config, ConfigException }

import scala.collection.JavaConverters._

/** Configuration for a single processor. */
case class ProcessorConfig(name: String, processor: Processor, inputs: Seq[ProcessorIo],
  outputs: Seq[ProcessorIo]) {
  /** @return true if this processor expects unnamed inputs */
  def wantsUnnamedInput: Boolean = {
    // Either the first (and by implication all subsequent) inputs are unnamed, or none are. We
    // consider empty inputs to be named.
    inputs.headOption map { _.isUnnamed } getOrElse { false }
  }
}
object ProcessorConfig {
  /** Builds a ProcessorConfig from a config with required key `name` and optional `inputs` and
    * `outputs` keys. If either of `inputs` or `outputs` are missing, they will
    * be filled with unnamed IO instances.
    */
  def fromConfig(config: Config)(implicit bindingModule: BindingModule): ProcessorConfig = {
    val processors = bindingModule.inject[Map[String, Processor]](None)

    if (!config.hasPath("name")) {
      throw new ErmineException(s"processor missing a 'name' key: ${config}")
    }
    val processorName = config.getString("name")
    val processor = processors.get(processorName) getOrElse {
      throw new ErmineException(s"unknown processor '${processorName}'")
    }

    val inputs = getIOValues(config, "inputs", processor.numInputs)
    val outputs = getIOValues(config, "outputs", processor.numOutputs)

    if (inputs.size != processor.numInputs) {
      throw new ErmineException(s"extrator ${processorName} requires ${processor.numInputs} " +
        s"inputs, has ${inputs.size} in the configuration")
    }
    if (outputs.size != processor.numOutputs) {
      throw new ErmineException(s"extrator ${processorName} requires ${processor.numOutputs} " +
        s"outputs, has ${outputs.size} in the configuration")
    }

    ProcessorConfig(processorName, processor, inputs, outputs)
  }

  /** Gets an input or output label set from a config using the given config path. This expects the
    * value at the given path to be an array of strings.
    * @throws ErmineException if the value at the path is not an array of strings
    */
  def getIOValues(config: Config, path: String, numExpected: Int): Seq[ProcessorIo] = {
    val configuredValues: Seq[ProcessorIo] = try {
      // Check for an object at the given path.
      if (config.hasPath(path)) {
        for {
          (configValue, index) <- config.getList(path).asScala.zipWithIndex
        } yield ProcessorIo.fromConfigValue(configValue, index)
      } else {
        Seq.empty
      }
    } catch {
      case e: ConfigException => {
        throw new ErmineException(s"bad ${path} value", e)
      }
    }

    if (configuredValues.size == 0) {
      // If there were no IO objects configured, use an unnamed IO (pipe from the previous
      // operation).
      for (i <- 0 until numExpected) yield ProcessorIo.unnamedIO(i.toString)
    } else {
      configuredValues
    }
  }
}
