package org.allenai.extraction.manager

import org.allenai.extraction.{ FlatProcessor, Processor }
import org.allenai.extraction.processors._

import com.escalatesoft.subcut.inject.NewBindingModule

import scala.io.Source

import java.io.Writer

/** Test processor that does nothing. */
object NoOpProcessor extends FlatProcessor {
  override protected def processInternal(source: Source, destination: Writer): Unit = { }
}

/** Test processor with two inputs and two outputs that cats the first input to the first output,
  * and cats the second input to the second output. */
object TwoByCatProcessor extends Processor {
  override val numInputs = 2
  override val numOutputs = 2
  override protected def processInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    for (line <- sources(0).getLines) {
      destinations(0).write(line + "\n")
    }
    for (line <- sources(1).getLines) {
      destinations(1).write(line + "\n")
    }
  }
}

/** Subcut module for testing. */
object TestErmineModule extends NewBindingModule(module => {
  // Available processors.
  module.bind [Map[String,Processor]] toSingle Map[String,Processor](
    "NoOpProcessor" -> NoOpProcessor,
    "CatProcessor" -> CatProcessor,
    "TwoByCatProcessor" -> TwoByCatProcessor,
    "TurtleProcessor" -> TurtleProcessor
  )
})
