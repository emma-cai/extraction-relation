package org.allenai.extraction.manager

import org.allenai.ari.datastore.client.{ AriDatastoreClient, AriDatastoreHttpClient }
import org.allenai.common.Config.EnhancedConfig
import org.allenai.extraction.ConfigModule
import org.allenai.extraction.Processor
import org.allenai.extraction.processors._
import org.allenai.extraction.processors.definition._
import org.allenai.extraction.processors.dependencies._

import akka.actor.ActorSystem
import akka.event.Logging
import com.escalatesoft.subcut.inject.NewBindingModule
import com.typesafe.config.Config

import scala.collection.mutable
import scala.io.Source

/** Module providing bindings for the Ermine system.
  * @param actorSystem the actor system, for logging and AriDatastoreClient
  */
class ErmineModule(actorSystem: ActorSystem) extends NewBindingModule(module => {
  import module._

  val log = Logging(actorSystem, classOf[ErmineModule])

  // Inject the config object we need.
  val config = ConfigModule.inject[Config](None)

  // Build up the supported processor map.
  bind[Map[String, Processor]] toModuleSingle { implicit module =>
    val processors: mutable.Map[String, Processor] = mutable.Map.empty
    def addProcessor(processor: Processor) = processors += processor.configMapping

    // Initialize with processors requiring no external configuration.
    addProcessor(CatProcessor)
    addProcessor(ClearSrl)
    addProcessor(CorpusSplitter)
    addProcessor(ExtractionLabels)
    addProcessor(ExtractionRoles)
    addProcessor(InferenceRules)
    addProcessor(MergeProcessor)
    addProcessor(OtterJsonToReadableOutputProcessor)
    addProcessor(StanfordExtractor)
    addProcessor(StanfordFixProcessor)
    addProcessor(StanfordTtl)
    addProcessor(TurtleProcessor)

    // Create the Ferret instance to use in our extractors, if we have a config key for it.
    config.get[String]("ferret.directory") match {
      case Some(ferretDir) => {
        val ferret = new Ferret(ferretDir)
        addProcessor(new FerretTextProcessor(ferret))
        addProcessor(new FerretQuestionProcessor(ferret))
      }
      case None =>
        log.error("ferret.directory not found in config - Ferret extractors won't be initialized")
    }

    // Get the data directory for extractors that need it and configure those extractors.
    config.get[String]("ermine.dataDirectory") match {
      case Some(dataDir) => {
        addProcessor(
          new ExtractionDenominalize(Source.fromFile(s"${dataDir}/wordnet-nominalizations.ttl")))
        val definitionDataDir = dataDir + "/" + "definitions"
        val glossary = config.get[String]("glossaryOfTerms")
        addProcessor(new OtterNounDefinitionExtractor(definitionDataDir, glossary))
      }
      case None => log.error("ermine.dataDirectory not found in config - " +
        "some extractors won't be initialized!")
    }

    // Configure the SimpleWiktionaryDefinitionPreprocessor.
    val simpleWiktionaryWordClasses: Set[String] =
      (config.get[Seq[String]]("simpleWiktionary.wordClasses") getOrElse { Seq.empty }).toSet
    addProcessor(new SimpleWiktionaryDefinitionPreprocessor(simpleWiktionaryWordClasses))

    // Configure the MultipleDictionarySourcePreprocessor.
    val multipleDictionaryWordClasses: Set[String] =
      (config.get[Seq[String]]("multipleDictionaries.wordClasses") getOrElse { Seq.empty }).toSet
    val multipleDictionarySources: Set[String] =
      (config.get[Seq[String]]("multipleDictionaries.dictionarySources") getOrElse { Seq.empty }).toSet
    addProcessor(new MultipleDictionarySourcePreprocessor(
      multipleDictionaryWordClasses, multipleDictionarySources))

    // Configure the OtterDefinitionDBWriter and OtterDefinitionDBWriter.
    val dbPathOption = config.get[String]("otterDB.dbPath")
    dbPathOption match {
      case (Some(dbPath)) =>
        val dbUserOption = config.get[String]("otterDB.dbUsername")
        val dbPasswordOption = config.get[String]("otterDB.dbPassword")
        addProcessor(new OtterDefinitionDBWriter(dbPath, dbUserOption, dbPasswordOption))
      case _ => log.error("dbPath is missing for OtterDefinitionDBWriter. " +
        "The processor failed to start up.")
    }

    // Bind the extractor map we built.
    processors.toMap
  }

  // Bind the ari datastore client, if it's configured.
  config.get[String]("aristore.url") match {
    case Some(datastoreUri) =>
      bind[AriDatastoreClient] toSingle new AriDatastoreHttpClient(datastoreUri)(actorSystem)
    case None => log.error("aristore.url not found in config - Aristore I/O won't be supported")
  }
})
