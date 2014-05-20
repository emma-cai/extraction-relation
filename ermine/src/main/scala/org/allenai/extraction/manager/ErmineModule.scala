package org.allenai.extraction.manager

import org.allenai.common.Config._
import org.allenai.extraction.ConfigModule
import org.allenai.extraction.Processor
import org.allenai.extraction.processors._

import com.escalatesoft.subcut.inject.NewBindingModule
import com.typesafe.config.Config

/** Module providing bindings for the Ermine system. */
object ErmineModule extends NewBindingModule(module => {
  import module._

  // Include Config bindings, and inject the config object we need.
  module <~ ConfigModule
  val config = inject[Config](None)

  // Create the Ferret instance to use in our extractors.
  val ferretDir = config[String]("ferret.directory")
  val ferret = new Ferret(ferretDir)

  // Get the data directory for the definition extractor
  val definitionsDataDir = config[String]("definitions.dataDirectory")

  // Get the set of wordclasses for the SimpleWiktionary preprocessor to operate on
  val simpleWiktionaryWordClasses =
    config.get[Seq[String]]("simpleWiktionary.wordClasses").getOrElse(Seq.empty[String]).toSet

  // Available extractors.
  bind[Map[String, Processor]] toSingle Map(
    "StanfordParser" -> StanfordParser,
    "FerretTextProcessor" -> new FerretTextProcessor(ferret),
    "FerretQuestionProcessor" -> new FerretQuestionProcessor(ferret),
    "StanfordXmlToTtl" -> StanfordXmlToTtl,
    "NounDefinitionOpenRegexExtractor" -> new NounDefinitionOpenRegexExtractor(definitionsDataDir),
    "SimpleWiktionaryDefinitionPreprocessor" -> new SimpleWiktionaryDefinitionPreprocessor(simpleWiktionaryWordClasses),
    "CatProcessor" -> CatProcessor)
})
