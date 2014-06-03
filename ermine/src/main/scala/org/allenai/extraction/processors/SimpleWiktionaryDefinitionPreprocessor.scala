package org.allenai.extraction.processors

import java.io.{ File, Writer }

import scala.io.Source
import scala.util.matching.Regex

import org.allenai.extraction.FlatProcessor

/** Preprocessor that takes Simple Wiktionary scraped text in the form: <term>\t<wordClass>\t<noisy-definition-text>
  * as input and converts it to the format expected by the Definition Extractor, which is:
  * <term>\t<wordClass>\t<cleaned-up-definition>.
  * @param wordClasses The set of wordclasses to consider. If this set is specified, i.e., non-empty, definitions
  *                 of terms belonging to any class outside of this set will NOT be processed/written out.
  *                 Default: Empty set, which means there are no filters, so the preprocessor will process/write out
  *                 all definitions of all word classes.
  */
class SimpleWiktionaryDefinitionPreprocessor(wordClasses: Set[String] = Set.empty) extends FlatProcessor {

  val wordClassesLowerCase = wordClasses.map(x => x.toLowerCase)

  /** The main extraction method: takes an Input Source with the scraped SimpleWiktionary text to
    * format and writes extraction output in the format: <term>\t<wordClass>\t<definition>,
    * as expected by the Definition extractor out to the specified Writer.
    */
  override protected def processText(input: Source, destination: Writer): Unit = {
    for {
      line <- input.getLines
      (term, termWordClass, termDefinition) <- breakLine(line)
      if (wordClassesLowerCase.contains(termWordClass.toLowerCase) || (wordClassesLowerCase.size == 0))
      cleanedUpDef <- cleanUp(termDefinition)
      if (cleanedUpDef.length() > 0)
    } destination.write(term + "\t" + termWordClass + "\t" + cleanedUpDef + "\n")
  }

  /** breakLine : Break the input line into its constituent parts.
    * SimpleWiktionary scraped text consists of one definition per line in a txt file,
    * with the format of <Term>\t<WordClass>\t<Definition>. The output of this preprocessor
    * will also be in the same format except the Definition will be cleaned up.
    */
  def breakLine(defnInputLine: String): Option[(String, String, String)] = {
    defnInputLine.split("\t") match {
      case Array(term, termWordClass, termDefinition, _*) => Some(term.trim, termWordClass.trim, termDefinition.trim)
      case _ => None
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
    * around	Preposition	# If a boat runs or goes 'ground', its bottom goes onto the ground
    *                       and it is not floating freely anymore. {{antonyms|afloat}}
    * keyboard	Noun	#{{context|music}} , {{countable}} A 'keyboard' is a range of black and white
    *                  keys (buttons) on a musical instrument.
    */
  def cleanUp(definitionRawLine: String): Seq[String] = {
    // Remove leading '#' character 
    val defPoundAtBeginningPattern = "^#".r
    val defBeginningPoundStripped: String = defPoundAtBeginningPattern replaceFirstIn (definitionRawLine, "")

    // Remove meta info - stuff in curly braces  if present : there could be a cluster of multiple of these
    // separated by semicolon or comma like the last e.g. in above documentation.
    val metaInfo = """\{\{[^}]*\}\}"""
    val defMetaInfoPattern = s"""${metaInfo}(?:\\s*(?:,|;)\\s*${metaInfo})*""".r
    val defPoundMetaStripped: String = defMetaInfoPattern replaceAllIn (defBeginningPoundStripped, "")

    // Remove all bracketed expressions
    val parenPattern = """\([^)]*\)""".r
    val defPoundMetaParenStripped: String = parenPattern replaceAllIn (defPoundMetaStripped, "")

    // Remove quotes from quoted words, like the definition term
    val defQuotedWordPattern = """(?<=\W|^)'([^']+)'(?=\W|$)""".r
    val defPoundMetaQuotesStripped = defQuotedWordPattern replaceAllIn (defPoundMetaParenStripped, m => m.group(1))

    // Break the line up into multiple definitions if separated by semicolons
    defPoundMetaQuotesStripped.split(";").toSeq map { x => x.trim }
  }
}
