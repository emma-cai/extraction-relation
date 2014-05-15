package org.allenai.extraction.manager

import org.allenai.extraction.{ FlatProcessor, Processor }

import com.escalatesoft.subcut.inject.NewBindingModule

import scala.io.Source

import java.io.Writer

/** Test processor that does nothing. */
object NoOpProcessor extends FlatProcessor {
  override protected def processInternal(source: Source, destination: Writer): Unit = { }
}

/** Subcut module for testing. */
object TestErmineModule extends NewBindingModule(module => {
  // Available processors.
  module.bind [Map[String,Processor]] toSingle Map[String,Processor](
    "NoOpProcessor" -> NoOpProcessor
  )
})
