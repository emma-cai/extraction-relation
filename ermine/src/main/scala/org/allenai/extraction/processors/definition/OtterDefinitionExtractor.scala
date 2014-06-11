package org.allenai.extraction.processors.definition

import java.io.Writer
import scala.io.Source
import spray.json.DefaultJsonProtocol._
import spray.json._
import org.allenai.extraction.processors.OpenRegexExtractor

/** A Case Class and Companion Object for Definition Extraction Output to support outputting results
  * in JSON.  The first three parameters come from the input and the last one contains the
  * extraction results.
  * @param term  Defined Term
  * @param wordClass Word Class for the term
  * @param definition The definition
  * @param results The resultant extractions
  */
case class OtterExtractionResult(term: String, wordClass: String, definition: String, results: Seq[String])

object OtterExtractionResult {
  import spray.json.DefaultJsonProtocol._
  implicit val definitionExtractionResultJsonFormat = jsonFormat4(OtterExtractionResult.apply)
}

/** An extractor that processes definitions for a given class of terms using OpenRegex.
  * A directory is expected per word class, with the cascade file in it being called defn.cascade.
  * wordClass can take different values like Noun, Adjective, Verb, Expression etc.
  * Definitions have different formats depending on the word class of the term being defined. This
  * will impact the rules defined in OpenRegex, so we will have a set of rules and a cascade file
  * for each word class.
  * @param dataPath  Path of the data directory that will contain OpenRegex rule files to be used
  * for the definition extraction.
  * @param wordClass The word class, for e.g., noun/verb/adjective to be processed. A subdirectory
  * is expected under the specified dataPath, for each word class. So the specified wordClass here
  * is appended to the dataPath to get to the necessary rule files.
  */
abstract class OtterDefinitionExtractor(dataPath: String, val wordClass: String) extends OpenRegexExtractor(dataPath + "//" + wordClass + "//defn.cascade") {

  /** The main extraction method: Input Source contains a bunch of definitions preprocessed into the format
    * represented by the PreprocessedDefinition structure. The preprocessed output serialized into JSONs,
    * is read here- from Source, and processed. The expected input structure is: "[" on the first line, followed
    * by one output (preprocessed definition) JSON per line separated by a "," per line, and a concluding "]" on
    * the last line. For e.g.:
    * [
    * {"definitionCorpusName":"SimpleWiktionary","rawDefinitionId":1,"rawDefinitionLine":"a priorichampionship\tNoun\t#\
    *  A 'championship' is a contest to decide which person or team is best at a sport.","definedTerm":"a priorichampionship",\
    *  "wordClass":"Noun","preprocessedDefinitions":["A championship is a contest to decide which person or team is best at a sport."],\
    *  "metaData":[]}
    * ,
    * {"definitionCorpusName":"SimpleWiktionary","rawDefinitionId":2,"rawDefinitionLine":"abacus\tNoun\t#{{countable}} An 'abacus' is an ancient calculating device. It is made of a frame with beads on various rods.","definedTerm":"abacus","wordClass":"Noun","preprocessedDefinitions":["An abacus is an ancient calculating device. It is made of a frame with beads on various rods."],"metaData":["countable"]}
    * ,
    * Output will be written out to the specified destination.
    */
  override protected def processText(defnInputSource: Source, destination: Writer): Unit = {
    //Start output Json 
    destination.write("[\n")
    // Iterate over input JSONs and process definitions.
    var beginning = true
    for (rawLine <- defnInputSource.getLines) {
      val line = rawLine.trim()
      // Skip over first line- it is expected to contain a "[",  the last line which is expected 
      // to contain a "]" and lines in the middle that have just a ",". 
      if (!(line.length == 0) && !line.equals("[") && !line.equals("]") && !line.equals(",")) {
        val preprocessedDefinitionsAst = line.parseJson
        val preprocessedDefinitions = preprocessedDefinitionsAst.convertTo[PreprocessedDefinition]
        for {
          preprocessedDefinition <- preprocessedDefinitions.preprocessedDefinitions
          termWordClass <- preprocessedDefinitions.wordClass
          if (termWordClass.equalsIgnoreCase(wordClass))
        } {
          val results = super.extractText(preprocessedDefinition)
          val extractionOp = OtterExtractionResult(
            preprocessedDefinitions.definedTerm, termWordClass, preprocessedDefinition, results)
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

  /** prerocessLine : Break the input line into its constituent parts.
    * The assumption here is that if a preprocessor for a given Definition corpus
    * was run earlier in the pipeline and feeds into this extractor (DefinitionOpenRegexExtractor),
    * then the input is of the form: <Term>\t<WordClass>\t<Definition> per line.
    * Alternately, this extractor can be run by itself on input consisting of just a <Definition>
    * per line.
    */
  def preprocessLine(defnInputLine: String): (String, String, String) = {
    defnInputLine.split("\t").toList match {
      // For handling output format from Preprocessor
      case List(term, termWordClass, termDefinition, _*) => (term.trim, termWordClass.trim, termDefinition.trim)
      // For handling just the raw definition- for e.g., when using the ermine service through the
      // web demo.
      case Nil :+ termDefinition => ("", "", termDefinition.trim)
      case _ => ("", "", "")
    }
  }
}
