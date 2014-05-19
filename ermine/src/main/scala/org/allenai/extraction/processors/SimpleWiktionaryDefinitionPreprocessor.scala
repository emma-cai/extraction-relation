package org.allenai.extraction.processors

import java.io.{ File, Writer }

import scala.io.Source
import scala.util.matching.Regex

import org.allenai.extraction.FlatProcessor

/** Preprocessor that takes Simple Wiktionary scraped text in the form: <term>\t<wordClass>\t<noisy-definition-text>
  * as input and converts it to the format expected by the Definition Extractor, which is:
  * <term>\t<wordClass>\t<cleaned-up-definition>.
  * @param wordClasses The set of wordclasses to consider- comes from application configuration.
  */
class SimpleWiktionaryDefinitionPreprocessor(wordClasses: Set[String]) extends FlatProcessor {

  lazy val wordClassesLowerCase = wordClasses.map(x => x.toLowerCase())

  /** The main extraction method: takes an Input Source with the scraped SimpleWiktionart text to
    * format and writes extraction output in the fromat: <term>\t<wordClass>\t<definition>,
    * as expected by the Definition extractor out to the specified Writer.
    */
  override protected def processInternal(input: Source, destination: Writer): Unit = {
    for (line <- input.getLines) {
      val (term, termWordClass, termDefinition) = breakLine(line)
      if (wordClassesLowerCase.contains(termWordClass.toLowerCase())) {
        val cleanedUpDefs: Seq[String] = cleanUp(termDefinition)
        for (cleanedUpDef <- cleanedUpDefs) {
          val finalFormattedDef: String = cleanedUpDef.trim()
          if (finalFormattedDef.length() > 0)
            destination.write(term + "\t" + termWordClass + "\t" + cleanedUpDef + "\n")
        }
      }
    }
  }

  /** breakLine : Break the input line into its constituent parts.
    * SimpleWiktionary scraped text consists of one definition per line in a txt file,
    * with the format of <Term>\t<WordClass>\t<Definition>. The output of this preprocessor
    * will also be in the same format except the Definition will be cleaned up.
    */
  def breakLine(defnInputLine: String): (String, String, String) = {
    defnInputLine.split("\t") match {
      case Array(term, termWordClass, termDefinition, _*) => (term.trim(), termWordClass.trim(), termDefinition.trim())
      case _ => ("", "", "")
    }
  }

  /** cleanUp : Break the input line into its constituent parts.
    * Cleaning up the input line will involve- Removing the leading '#' character, the meta
    * information enclosed in curly braces and quotes enclosing words and breaking the definition up
    * into multiple definitions when there are different interpretations in the same line,
    * to return them as separate definitions, hence the Seq return type.
    * Sample SimpleWiktionary Definition lines:
    * abandon	Noun	#{{uncountable}} 'Abandon' is a state where you do not control yourself.
    * aberrant	Adjective	# Straying from the right or usual course; wandering.
    * aground	Preposition	# If a boat runs or goes 'aground', its bottom goes onto the ground
    *                            and it is not floating freely anymore. {{antonyms|afloat}}
    */
  def cleanUp(definitionRawLine: String): Seq[String] = {
    // Remove leading '#' character 
    val defPoundAtBeginningPattern = new Regex("^#")
    val defBeginningPoundStripped: String = defPoundAtBeginningPattern replaceFirstIn (definitionRawLine, "")

    // Remove meta info - stuff in curly braces  if present
    val defMetaInfoPattern = new Regex("\\{\\{.+\\}\\}")
    val defPoundMetaStripped: String = defMetaInfoPattern replaceAllIn (defBeginningPoundStripped, "")

    // Remove quotes from quoted words, like the definition term
    val defQuotedWordPattern = new Regex("(\\W)'(.+)'(\\W)", "beforeTerm", "term", "afterTerm")
    val defPoundMetaQuotesStripped = defQuotedWordPattern replaceAllIn (defPoundMetaStripped,
      m => m.group("beforeTerm") + m.group("term") + m.group("afterTerm"))

    // Break the line up into multiple definitions if separated by semicolons
    defPoundMetaQuotesStripped.split(";").toSeq
  }
}
