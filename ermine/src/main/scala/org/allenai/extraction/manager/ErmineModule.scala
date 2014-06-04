package org.allenai.extraction.manager

import org.allenai.ari.datastore.client.{ AriDatastoreClient, AriDatastoreHttpClient }
import org.allenai.common.Config._
import org.allenai.extraction.{ ConfigModule, Processor }
import org.allenai.extraction.processors._

import akka.actor.ActorSystem
import akka.event.Logging
import com.escalatesoft.subcut.inject.NewBindingModule
import com.typesafe.config.Config

import scala.collection.mutable

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
    // Initialize with processors requiring no external configuration.
    val processors = mutable.Map[String, Processor](
      "StanfordParser" -> StanfordParser,
      "StanfordTtl" -> StanfordTtl,
      "StanfordXmlToTtl" -> StanfordXmlToTtl,
      "CatProcessor" -> CatProcessor)

    // Create the Ferret instance to use in our extractors, if we have a config key for it.
    config.get[String]("ferret.directory") match {
      case Some(ferretDir) => {
        val ferret = new Ferret(ferretDir)
        processors += ("FerretTextProcessor" -> new FerretTextProcessor(ferret))
        processors += ("FerretQuestionProcessor" -> new FerretQuestionProcessor(ferret))
      }
      case None =>
        log.error("ferret.directory not found in config - Ferret extractors won't be initialized")
    }

    // Configure the Get the data directory for the definition extractor.
    config.get[String]("definitions.dataDirectory") match {
      case Some(dataDir) => processors += (
          "NounDefinitionOpenRegexExtractor" -> new NounDefinitionOpenRegexExtractor(dataDir)
      )
      case None => log.error("definitions.dataDirectory not found in config - " +
        "NounDefinitionOpenRegexExtractor won't be initialized")
    }

    // Configure the SimpleWiktionaryDefinitionPreprocessor.
    val wordClasses: Set[String] =
      (config.get[Seq[String]]("simpleWiktionary.wordClasses") getOrElse { Seq.empty }).toSet
    processors += ("SimpleWiktionaryDefinitionPreprocessor" ->
      new SimpleWiktionaryDefinitionPreprocessor(wordClasses))

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
