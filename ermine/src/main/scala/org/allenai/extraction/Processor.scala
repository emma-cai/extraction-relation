package org.allenai.extraction

import scala.io.Source

import java.io.Writer

/** A processor that converts data from one format into another. */
abstract class Processor {
  /** @return the number of sources this processor expects */
  def numInputs: Int

  /** @return the number of destinations this processor expects */
  def numOutputs: Int

  /** Process data from the given input(s), writing to the given output(s). */
  final def process(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    require(sources.size == numInputs, s"${getClass.getSimpleName} expects ${numInputs} input(s)")
    require(destinations.size == numOutputs,
      s"${getClass.getSimpleName} expects ${numOutputs} output(s)")

    processInternal(sources, destinations)
  }

  /** Process data from the given input(s), writing to the given output(s). */
  protected def processInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit
}

/** A processor with only one input and output. */
abstract class FlatProcessor extends Processor {
  override val numInputs = 1
  override val numOutputs = 1
  override protected def processInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    processInternal(sources.head, destinations.head)
  }

  /** Process data from the given input, writing to the given output. */
  protected def processInternal(source: Source, destination: Writer): Unit
}
