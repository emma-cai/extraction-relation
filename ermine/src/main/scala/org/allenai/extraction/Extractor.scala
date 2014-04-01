package org.allenai.extraction

import scala.io.Source

import java.io.Writer

/** An extractor that processes data from one format into another. */
abstract class Extractor {
  /** Process data from the given input, writing to the given output. */
  def extract(source: Source, destination: Writer): Unit
}
