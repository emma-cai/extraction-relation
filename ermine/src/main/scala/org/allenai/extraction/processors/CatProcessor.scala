package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor

import scala.io.Source

import java.io.Writer

/** A demo processor that writes its input to its output (like the commandline tool `cat`). */
object CatProcessor extends TextProcessor {
  // Processors must override these two fields. These are used for error-checking of pipeline
  // configurations - processText will not be called except with `sources` and `destinations` of
  // exactly the sizes given below.
  override val numInputs = 1
  override val numOutputs = 1

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    // Within this function, you have access to the inputs and outputs you configured in your
    // processor configuration, in the order they were declared.
    //
    // If you didn't configure any inputs, the previous pipeline's outputs will be used, in the
    // order they were declared. If there aren't enough outputs from the previous stage to satisfy
    // your processor, the pipeline won't run.
    //
    // If you didn't configure any outputs, enough will be provided for you to write to, and will be
    // available only to the next pipeline stage.
    val source = sources(0)
    val sink = destinations(0)
    // This pipes the source data right back out - not terribly useful. Although you could implement
    // a slow 'cp' command by configuring different input and output files . . .
    for (line <- source.getLines) {
      sink.write(line)
      sink.write("\n")
    }
  }
}
