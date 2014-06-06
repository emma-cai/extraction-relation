package org.allenai.extraction.processors.definition

import java.io.Writer
import scala.io.Source
import spray.json.DefaultJsonProtocol._
import spray.json.pimpAny
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

  /** The main extraction method: Input Source contains a bunch of definitions preprocessed
    * into the format: <term>\t<wordClass>\t<definition>. Output will be written out to the
    * specified destination
    */
  override protected def processText(defnInputSource: Source, destination: Writer): Unit = {
    //Start output Json 
    destination.write("[")
    // Iterate over input sentences (definitions), preprocess each and send it to the extractText
    // method.
    var beginning = true
    for (line <- defnInputSource.getLines) {
      val (term, termWordClass, termDefinition) = preprocessLine(line)
      if (termWordClass.equalsIgnoreCase(wordClass)) {
        val results = super.extractText(termDefinition)
        val extractionOp = OtterExtractionResult(term, termWordClass, termDefinition, results)
        if (!beginning) {
          destination.write(",")
        }
        destination.write(extractionOp.toJson.prettyPrint)
        if (beginning) {
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
