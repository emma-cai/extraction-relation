package org.allenai.extraction.manager

import org.allenai.extraction.{ ErmineException, Processor }
import org.allenai.extraction.manager.io._
import org.allenai.extraction.processors._

import com.escalatesoft.subcut.inject.BindingModule
import com.typesafe.config.{ Config, ConfigValue, ConfigException, ConfigValueFactory }

import scala.collection.JavaConverters._

/** Configuration for a single processor. */
case class ProcessorConfig(name: String, processor: Processor, inputs: Seq[ProcessorInput],
    outputs: Seq[PipelineOutput]) {
  /** @return true if this processor expects unnamed inputs */
  def wantsUnnamedInput: Boolean = {
    // Either the first (and by implication all subsequent) inputs are unnamed, or none are. We
    // consider empty inputs to be named.
    inputs.headOption match {
      case Some(UnnamedInput()) => true
      case _ => false
    }
  }

  /** @return a copy of this processor config with initialized inputs and outputs */
  def getInitializedCopy()(implicit bindingModule: BindingModule): ProcessorConfig = {
    val initializedInputs = inputs map { _.initialize() }
    val initializedOutputs = outputs map { _.initialize() }
    ProcessorConfig(name, processor, initializedInputs, initializedOutputs)
  }
}
object ProcessorConfig {
  /** Empty-object config value used to build IO objects when their config is missing. */
  val EmptyConfigValue: ConfigValue = ConfigValueFactory.fromAnyRef(Map.empty.asJava)

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

    val inputs = getIOValues(config, "inputs", processor.numInputs) {
      ProcessorInput.fromConfigValue
    }
    val outputs = getIOValues(config, "outputs", processor.numOutputs) {
      PipelineOutput.fromConfigValue
    }

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
  def getIOValues[T](config: Config, path: String, numExpected: Int)(
    factory: ConfigValue => T): Seq[T] = {

    val configuredValues: Seq[T] = try {
      // Check for an object at the given path.
      if (config.hasPath(path)) {
        for {
          configValue <- config.getList(path).asScala
        } yield factory(configValue)
      } else {
        Seq.empty
      }
    } catch {
      case e: ConfigException => {
        throw new ErmineException(s"bad ${path} value", e)
      }
    }

    if (configuredValues.size == 0) {
      // If there were no IO objects configured, use defaults.
      for (_ <- 0 until numExpected) yield factory(EmptyConfigValue)
    } else {
      configuredValues
    }
  }
}
