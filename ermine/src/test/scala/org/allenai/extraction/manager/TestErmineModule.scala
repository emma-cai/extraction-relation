package org.allenai.extraction.manager

import org.allenai.extraction.{ Extractor, FlatExtractor }

import com.escalatesoft.subcut.inject.NewBindingModule

import scala.io.Source

import java.io.Writer

/** Test extractor that does nothing. */
object NoOpExtractor extends FlatExtractor {
  override protected def extractInternal(source: Source, destination: Writer): Unit = { }
}

/** Subcut module for testing. */
object TestErmineModule extends NewBindingModule(module => {
  // Available extractors.
  module.bind [Map[String,Extractor]] toSingle Map[String,Extractor](
    "NoOpExtractor" -> NoOpExtractor
  )
})
