package org.allenai.extraction.manager

import org.allenai.common.Config._
import org.allenai.extraction.ConfigModule
import org.allenai.extraction.Extractor
import org.allenai.extraction.extractors._

import com.escalatesoft.subcut.inject.NewBindingModule
import com.typesafe.config.Config

/** Module providing bindings for the Ermine system. */
object ErmineModule extends NewBindingModule (module => {
  import module._

  // Include Config bindings, and inject the config object we need.
  module <~ ConfigModule
  val config = inject[Config](None)

  // Create the Ferret instance to use in our extractors.
  val ferretDir = config[String]("ferret.directory")
  val ferret = new Ferret(ferretDir)

  // Available extractors.
  bind [Map[String,Extractor]] toSingle Map(
    "StanfordParser" -> StanfordParser,
    "FerretTextExtractor" -> new FerretTextExtractor(ferret),
    "FerretQuestionExtractor" -> new FerretQuestionExtractor(ferret),
    "StanfordXmlToTtl" -> StanfordXmlToTtl,
    "FerretToExtractionRule" -> FerretToExtractionRule
  )
})
