package org.allenai.extraction.processors.definition

import org.allenai.ari.datastore.client.AriDatastoreClient
import org.allenai.ari.datastore.interface.{ Dataset, DocumentType, FileDocument, TextFile }
import org.allenai.extraction.confidence.{ ExtractionConfidenceFunction, OtterExtractionTupleAnnotated }
import org.allenai.extraction.manager.io._
import org.allenai.extraction.processors.OpenRegexExtractor

import akka.actor.ActorSystem
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable, NewBindingModule }
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import spray.json._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.io.Source

import java.io.File
import java.io.Writer
import java.net.URI
import java.nio.file.Files

/** An extractor that processes definitions for a given class of terms using OpenRegex.
  * A directory is expected per word class, with the cascade file in it being called defn.cascade.
  * wordClass can take different values like Noun, Adjective, Verb, Expression etc.
  * Definitions have different formats depending on the word class of the term being defined.
  * This will impact the rules defined in OpenRegex, so we will have a set of rules and a cascade file
  * for each word class.
  * @param dataPath  path of the data directory that will contain OpenRegex rule files to be used for
  * the definition extraction.
  * @param wordClass word class, for e.g., noun/verb/adjective to be processed. A subdirectory is
  * expected under the specified dataPath, for each word class. So the specified wordClass here is
  * appended to the dataPath to get to the necessary rule files.
  * @param glossary optional path to aristore file containing the set of required terms- anything
  * outside of this set has to be filtered from processing. None implies Empty set, which means
  * "no filters", so all terms will be included in that case.
  */
abstract class OtterDefinitionExtractor(
  dataPath: String, val wordClass: String, glossaryOption: Option[String] = None)(implicit val bindingModule: BindingModule)
    extends OpenRegexExtractor[OtterExtractionTuple](dataPath + "//" + wordClass + "//defn.cascade")
    with Injectable {

  // Get glossary of terms from the glossary file (if specified), in Aristore.
  val aristoreClient = inject[AriDatastoreClient]
  val aristoreTimeout = 10.minutes

  // The actor system to pull an execution context out of.
  val actorSystem = inject[ActorSystem]
  // Import the actor system's execution context.
  import actorSystem.dispatcher

  val glossaryTerms = (for {
    glossary <- glossaryOption
    (datasetId, documentId) <- aristoreDataSetDocument(glossary)
  } yield {
    val tempDirectory = Files.createTempDirectory(datasetId).toFile
    tempDirectory.deleteOnExit
    for {
      dataset <- aristoreClient.getDataset(datasetId)
      document <- aristoreClient.getFileDocument[TextFile](dataset.id, documentId, tempDirectory)
    } yield {
      Source.fromFile(document.file).getLines
    }
  }) match {
    case Some(lines) => Await.result(lines, aristoreTimeout).toSet
    case _ => Set.empty[String]
  }

  /** Helper Method that returns None if the UriInput object being checked is not an AristoreFileInput
    */
  def aristoreDataSetDocument(path: String): Option[(String, String)] = {
    URI.create(path).getPath.stripPrefix("/").split("/") match {
      case Array(datasetId, documentId) => Some((datasetId, documentId))
      case _ => None
    }
  }
  
  /** The main extraction method: Input Source contains a bunch of definitions preprocessed into the format
    * represented by the PreprocessedDefinition structure. The preprocessed output serialized into JSONs,
    * is read here- from Source, and processed. The expected input structure is: "[" on the first line, followed
    * by one output (preprocessed definition) JSON per line separated by a "," per line, and a concluding "]" on
    * the last line. For e.g.:
    * (format: OFF)
    * [
    *   { "definitionCorpusName":"SimpleWiktionary","rawDefinitionId":1,"rawDefinitionLine":"a priorichampionship\tNoun\t#\
    *     A 'championship' is a contest to decide which person or team is best at a sport.","definedTerm":"a priorichampionship",\
    *     "wordClass":"Noun","preprocessedDefinitions":["A championship is a contest to decide which person or team is best at a sport."],\
    *     "metaData":[] }
    *   ,
    *   { "definitionCorpusName":"SimpleWiktionary","rawDefinitionId":2,"rawDefinitionLine":"abacus\tNoun\t#{{countable}} An 'abacus' is an \
    *     ancient calculating device. It is made of a frame with beads on various rods.","definedTerm":"abacus","wordClass":"Noun",\
    *     "preprocessedDefinitions":["An abacus is an ancient calculating device. It is made of a frame with beads on various rods."],\
    *     "metaData":["countable"] }
    * ]
    * (format: ON)
    * Output will be written out to the specified destination.
    */
  override def processText(defnInputSource: Source, destination: Writer): Unit = {
    val confidenceFun = ExtractionConfidenceFunction.loadDefaultOtterClassifier()
    //Start output Json 
    destination.write("[\n")
    var beginning = true
    // Iterate over input JSONs and process definitions.
    for (rawLine <- defnInputSource.getLines) {
      val line = rawLine.trim()
      // Skip over first line- it is expected to contain a "[",  the last line which is expected 
      // to contain a "]" and lines in the middle that have just a ",". 
      if (!(line.length == 0) && !line.equals("[") && !line.equals("]") && !line.equals(",")) {
        val preprocessedDefinitionsAst = line.parseJson
        val preprocessedDefinitionAlts = preprocessedDefinitionsAst.convertTo[PreprocessedDefinition]
        if (glossaryTerms.isEmpty || 
                glossaryTerms.contains(preprocessedDefinitionAlts.definedTerm.trim.toLowerCase)) {
          var otterExtractionsForDefinitionAlternates = Seq.empty[OtterExtractionForDefinitionAlternate]
          for {
             preprocessedDefinition <- preprocessedDefinitionAlts.preprocessedDefinitions
             if preprocessedDefinition.length > 0
             termWordClass <- preprocessedDefinitionAlts.wordClass
             if termWordClass.equalsIgnoreCase(wordClass)
          }  {
            val (tuples, tokensIn) = extract(preprocessedDefinitionAlts.definedTerm, preprocessedDefinition)
            val otterTokens = OtterToken.makeTokenSeq(tokensIn)
            val scoredResults = for {
              extr <- tuples
              rel = (extr.relation.relationType map (x => x.toString.toLowerCase)).getOrElse("")
              } yield {
                val score: Option[Double] = if (rel != "isa") {
                  None 
                } else {
                  Some(confidenceFun(rel)(OtterExtractionTupleAnnotated(extr, otterTokens)))
                }
                ScoredOtterExtractionTuple(extr, score)
            }
            otterExtractionsForDefinitionAlternates :+= OtterExtractionForDefinitionAlternate(
                                             preprocessedDefinition,
                                             otterTokens,
                                             scoredResults)    
          }
          // Compose the OtterExtraction overall result per raw definition to be written out as json.
          val extractionOp = OtterExtraction(
                               preprocessedDefinitionAlts.definitionCorpusName,
                               preprocessedDefinitionAlts.rawDefinitionId,
                               preprocessedDefinitionAlts.rawDefinition,
                               preprocessedDefinitionAlts.definedTerm,
                               preprocessedDefinitionAlts.wordClass,
                               otterExtractionsForDefinitionAlternates)
          if (!beginning) {
            destination.write(",\n")
          }
          destination.write(extractionOp.toJson.compactPrint + "\n")
          beginning = false
        }
      }
    }    
    // End output Json
    destination.write("]")
  }

  /** Method that takes a definition alternate, calls the necessary functions on it to perform
    * extraction and returns a sequence of extractions as OtterExtractionTuples which are to be
    * composed into an overall OtterExtraction for a single original raw definition from the corpus.
    */
  def extract(definedTerm: String, preprocessedDefinition: String) : (Seq[OtterExtractionTuple], Seq[Lemmatized[ChunkedToken]]) = {
    super.extractText(preprocessedDefinition)
  }
}
