package org.allenai.extraction.processors.definition

import spray.json.DefaultJsonProtocol._

/** A Case Class to represent a preprocessed raw definition
  * @param definitionCorpusName name/path or any identifier for the input definition corpus if available.
  * @param rawDefinitionId just a sequential index for a raw definition within the given corpus.
  * @param rawDefinitionLine raw Definition line (input).
  * @param definedTerm  term.
  * @param wordClass word class for the term, for e.g., Noun, Verb etc. usually available in
  * any standard definition corpus.
  * @param preprocessedDefinitions formatted definitions output by the preprocessor. This is a Seq because
  * a certain (raw definition) line might be split into multiple definitions when something is paraphrased.
  * @param metaData any additional metadata available, for e.g., context, and other attributes,
  * like 'countable' for nouns, etc.				
  *
  * For e.g., input raw definition line (line # 214) from SimpleWiktionary.20140114.txt: 
  * "Academia        noun        #{{uncountable}} 'Academia' is a word for the group of people 
  * who are a part of the scientific and cultural community; this group of people have 
  * attended a university and/or do research." gets split into two definitions during 
  * preprocessing. This will produce:
  * (format: OFF)
  * PreprocessedDefinition {
  *    definitionCorpusName: "SimpleWiktionary.20140114.txt"
  *    rawDefinitionId : 214
  *    rawDefinitionLine: "Academia        noun        #{{uncountable}} 'Academia' is a word 
  *    for the group of people who are a part of the scientific and cultural community; this 
  *    group  of people have attended a university and/or do research."
  *    definedTerm : "Academia"
  *    wordClass : "noun"
  *    preprocessedDefinitions: Seq[String] {
  *    "Academia is a word for the group of people who are a part of the scientific 
  *     and cultural community",
  *    "this group of people have attended a university and/or do research."
  *     }
  *    metaData: Seq[String] {
  *    "uncountable"
  *    }
  *  }
  * (format: ON)
  */
case class PreprocessedDefinition (
    definitionCorpusName: Option[String],
    rawDefinitionId: Int,
    rawDefinition: String,
    definedTerm: String,
    wordClass: Option[String],
    preprocessedDefinitions: Seq[String],
    metaData: Seq[String])
 

/** Companion Object to support conversion to/from JSON.
  */
object PreprocessedDefinition {
  import spray.json.DefaultJsonProtocol._
  implicit val preprocessedDefinitionJsonFormat = jsonFormat7(PreprocessedDefinition.apply)
} 
