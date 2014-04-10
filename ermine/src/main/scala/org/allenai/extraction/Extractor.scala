package org.allenai.extraction

import scala.io.Source

import java.io.Writer

/** An extractor that processes data from one format into another. */
abstract class Extractor {
  /** @return the number of sources this extractor expects */
  def numInputs: Int

  /** @return the number of destinations this extractor expects */
  def numOutputs: Int

  /** Process data from the given input(s), writing to the given output(s). */
  final def extract(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    require(sources.size == numInputs, s"${getClass.getSimpleName} expects ${numInputs} input(s)")
    require(destinations.size == numOutputs,
      s"${getClass.getSimpleName} expects ${numOutputs} output(s)")

    extractInternal(sources, destinations)
  }

  /** Process data from the given input(s), writing to the given output(s). */
  protected def extractInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit
}

/** An extractor with only one input and output. */
abstract class FlatExtractor extends Extractor {
  override val numInputs = 1
  override val numOutputs = 1
  override protected def extractInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    extractInternal(sources.head, destinations.head)
  }

  /** Process data from the given input, writing to the given output. */
  protected def extractInternal(source: Source, destination: Writer): Unit
}
