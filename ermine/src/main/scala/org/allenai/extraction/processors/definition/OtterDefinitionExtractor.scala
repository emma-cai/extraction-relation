package org.allenai.extraction.processors.definition

import org.allenai.extraction.processors.OpenRegexExtractor

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized

import java.io.Writer

import scala.io.Source

import spray.json.pimpAny
import spray.json.pimpString

/** An extractor that processes definitions for a given class of terms using OpenRegex.
  * A directory is expected per word class, with the cascade file in it being called defn.cascade.
  * wordClass can take different values like Noun, Adjective, Verb, Expression etc.
  * Definitions have different formats depending on the word class of the term being defined. This will
  * impact the rules defined in OpenRegex, so we will have a set of rules and a cascade file for each word class.
  * @param dataPath  path of the data directory that will contain OpenRegex rule files to be used for the definition extraction.
  * @param wordClass word class, for e.g., noun/verb/adjective to be processed. A subdirectory is expected under the specified dataPath,
  * for each word class. So the specified wordClass here is appended to the dataPath to get to the necessary rule files.
  * @param glossaryTerms a set of required terms- anything outside of this set has to be filtered
  * from processing.
  */
abstract class OtterDefinitionExtractor(
  dataPath: String, val wordClass: String, glossaryTerms: Set[String] = Set.empty[String])
    extends OpenRegexExtractor[OtterExtractionTuple](dataPath + "//" + wordClass + "//defn.cascade") {
  
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
        var otterExtractionsForDefinitionAlternates = Seq.empty[OtterExtractionForDefinitionAlternate]
        for {
           preprocessedDefinition <- preprocessedDefinitionAlts.preprocessedDefinitions
           termWordClass <- preprocessedDefinitionAlts.wordClass
           if (preprocessedDefinition.trim.length > 0 &&
               termWordClass.equalsIgnoreCase(wordClass) &&
               (glossaryTerms.isEmpty || 
                glossaryTerms.contains(preprocessedDefinitionAlts.definedTerm.trim.toLowerCase)))
        }  {
          val result = extract(preprocessedDefinitionAlts.definedTerm, preprocessedDefinition)
          otterExtractionsForDefinitionAlternates :+= OtterExtractionForDefinitionAlternate(
                                             preprocessedDefinition.trim,
                                             OtterToken.makeTokenSeq(result._2),
                                             result._1)
        
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
