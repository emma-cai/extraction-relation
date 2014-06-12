package org.allenai.extraction

import scala.io.Source

import java.io.{ File, FileWriter, Writer }

/** A processor that converts data from one format into another. */
abstract class Processor {

  /** @return the number of sources this processor expects */
  def numInputs: Int

  /** @return the number of destinations this processor expects */
  def numOutputs: Int

  /** Process data from the given input(s), writing to the given output(s). */
  final def process(sources: Seq[Processor.Input], destinations: Seq[Processor.Output]): Unit = {
    require(sources.size == numInputs, s"${getClass.getSimpleName} expects ${numInputs} input(s)")
    require(destinations.size == numOutputs,
      s"${getClass.getSimpleName} expects ${numOutputs} output(s)")

    processInternal(sources, destinations)
  }

  /** Process data from the given input(s), writing to the given output(s). */
  protected def processInternal(sources: Seq[Processor.Input],
    destinations: Seq[Processor.Output]): Unit
}
object Processor {
  /** An input for a processor. One configured input will normally produce one source, as in the
    * class SingleInput. This is provided to allow for a single configured input to produce
    * multiple input streams, such as with a local directory or an Aristore Dataset.
    *
    * Sources should have a description that is usable as a filename.
    */
  trait Input {
    /** @return the list of Sources the processor should read from */
    def getSources(): Seq[Source]
  }

  trait SingleInput extends Input {
    /** @return the Source the processor should read from */
    def getSource(): Source

    override final def getSources(): Seq[Source] = Seq(getSource())
  }

  /** An input using a predefined Source. Used for passing input directly into a pipeline. */
  class SourceInput(val source: Source) extends SingleInput {
    override def getSource() = source
  }

  /** An output for a processor. This will provide either a single file to write to, or a directory
    * to create one or more output files in.
    */
  trait Output {
    /** @return a File to write output data to. This will be re-used to read data from if later
      * processors require it. This may point to either a regular file or a directory.
      */
    def getOutputFile(): File
  }
}

/** A processor base class that creates file writers from the given outputs.  Most processors (those
  * that don't need to create new output files or generate non-text outputs) will extend this.
  */
abstract class TextProcessor extends Processor {
  /** Process data from the given input(s), writing to the given output(s). */
  override protected def processInternal(sources: Seq[Processor.Input],
    destinations: Seq[Processor.Output]): Unit = {

    val sourceInstances = (sources map { _.getSources() }).flatten
    val destinationWriters = for {
      destination <- destinations
      outputFile = destination.getOutputFile
    } yield {
      if (outputFile.isDirectory) {
        throw new ErmineException("TextProcessor requires all outputs to be non-directories!")
      }
      new FileWriter(outputFile)
    }
    if (sourceInstances.size != sources.size) {
      throw new ErmineException("TextProcessor requires all inputs to be non-directories!")
    }

    processText(sourceInstances, destinationWriters)

    sourceInstances foreach { _.close }
    destinationWriters foreach { writer =>
      writer.flush
      writer.close
    }
  }

  /** Process data from the given input(s), writing to the given text stream output. */
  def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit
}

/** A text processor with only one input and output. */
abstract class FlatProcessor extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1
  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    processText(sources.head, destinations.head)
  }

  /** Process data from the given input, writing to the given output. */
  def processText(source: Source, destination: Writer): Unit
}

/** A text processor with one input and output which can be either a pair of
  * singular files or a pair of directories.
  */
abstract class MultiTextProcessor extends Processor {
  override val numInputs = 1
  override val numOutputs = 1

  /** Process data from the given input(s), writing to the given output(s). */
  override protected def processInternal(sources: Seq[Processor.Input],
    destinations: Seq[Processor.Output]): Unit = {

    val sourceInstances = sources(0).getSources()
    val destinationFile = destinations(0).getOutputFile()
    // Verify that, if we were given multiple sources, we can create enough outputs to satisfy them
    // all.
    if (sourceInstances.size > 1 && !destinationFile.isDirectory()) {
      throw new ErmineException(
        "MultiTextProcessor given multiple inputs, but a non-directory output!")
    }
    val destinationWriters = if (destinationFile.isDirectory()) {
      sourceInstances map { source => new FileWriter(new File(destinationFile, source.descr)) }
    } else {
      Seq(new FileWriter(destinationFile))
    }

    for ((source, destination) <- sourceInstances zip destinationWriters) {
      processText(source, destination)
    }

    sourceInstances foreach { _.close }
    destinationWriters foreach { writer =>
      writer.flush
      writer.close
    }
  }

  /** Process data from the given input, writing to the given output. */
  def processText(source: Source, destination: Writer): Unit
}
