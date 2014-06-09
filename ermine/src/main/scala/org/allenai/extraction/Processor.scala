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
  final def process(sources: Seq[Source], destinations: Seq[Processor.Output]): Unit = {
    require(sources.size == numInputs, s"${getClass.getSimpleName} expects ${numInputs} input(s)")
    require(destinations.size == numOutputs,
      s"${getClass.getSimpleName} expects ${numOutputs} output(s)")

    processInternal(sources, destinations)
  }

  /** Process data from the given input(s), writing to the given output(s). */
  protected def processInternal(sources: Seq[Source], destinations: Seq[Processor.Output]): Unit
}
object Processor {
  trait Output {
    /** @return a File to write output data to. This will be re-used to read data from if later
      * processors require it.
      */
    def getOutputFile(): File
  }
}

/** A processor base class that creates file writers from the given outputs.  Most processors (those
  * that don't need to create new output files or generate non-text outputs) will extend this.
  */
abstract class TextProcessor extends Processor {
  /** Process data from the given input(s), writing to the given output(s). */
  protected def processInternal(sources: Seq[Source], destinations: Seq[Processor.Output]): Unit = {
    val destinationWriters = for {
      destination <- destinations
    } yield new FileWriter(destination.getOutputFile)

    processText(sources, destinationWriters)

    destinationWriters foreach { writer =>
      writer.flush
      writer.close
    }
  }

  /** Process data from the given input(s), writing to the given text stream output. */
  protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit
}

/** A processor with only one input and output. */
abstract class FlatProcessor extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1
  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    processText(sources.head, destinations.head)
  }

  /** Process data from the given input, writing to the given output. */
  protected def processText(source: Source, destination: Writer): Unit
}
