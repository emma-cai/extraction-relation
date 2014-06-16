package org.allenai.extraction.processors

import org.allenai.extraction.FlatProcessor
import org.allenai.extraction.processors.definition.PreprocessedDefinition

import java.io.Writer

import scala.io.Source

import spray.json.pimpAny

/** Preprocessor that takes Simple Wiktionary scraped text in the form: <term>\t<wordClass>\t<noisy-definition-text>
  * as input and converts it to the format expected by the Definition Extractor, which is:
  * <term>\t<wordClass>\t<cleaned-up-definition>.
  * @param wordClasses the set of wordclasses to consider. If this set is specified, i.e., non-empty, definitions
  * of terms belonging to any class outside of this set will NOT be processed/written out.
  * Default: Empty set, which means there are no filters, so the preprocessor will process/write out
  * all definitions of all word classes.
  */
class SimpleWiktionaryDefinitionPreprocessor(wordClasses: Set[String] = Set.empty) extends FlatProcessor {

  val wordClassesLowerCase = wordClasses.map(x => x.toLowerCase)

  /** The main extraction method: takes an Input Source with the scraped SimpleWiktionary text to
    * format, and writes extraction output in the format defined by the PreprocessedDefinition class.
    */
  override def processText(input: Source, destination: Writer): Unit = {
    //Start output Json 
    destination.write("[\n")
    var beginning = true
    var defId = 1
    for {
      line <- input.getLines
      (term, termWordClass, termDefinition) <- breakLine(line)
      if (wordClassesLowerCase.contains(termWordClass.toLowerCase) || (wordClassesLowerCase.size == 0))
    } {
      val (defAlts, metaData) = cleanUp(termDefinition)
      val preprocessedDefOp =
        PreprocessedDefinition(Some("SimpleWiktionary"), defId, line, term, Some(termWordClass), defAlts, metaData)
      if (!beginning) {
        destination.write(",\n")
      }
      destination.write(preprocessedDefOp.toJson.compactPrint + "\n")
      beginning = false
      defId += 1
    }
    // End output Json
    destination.write("]")
  }

  /** breakLine : Break the input line into its constituent parts.
    * SimpleWiktionary scraped text consists of one definition per line in a txt file,
    * with the format of <Term>\t<WordClass>\t<Definition>.
    */
  def breakLine(defnInputLine: String): Option[(String, String, String)] = {
    defnInputLine.split("\t") match {
      case Array(term, termWordClass, termDefinition, _*) => Some((term.trim, termWordClass.trim, termDefinition.trim))
      case _ => None
    }
  }

  /** cleanUp : Break the input line into its constituent parts.
    * Cleaning up the input line will involve- Removing the leading '#' character, the meta
    * information enclosed in curly braces and quotes enclosing words and breaking the definition up
    * into multiple definitions when there are different interpretations in the same line,
    * to return them as separate definitions, hence the Seq return type.
    * Sample SimpleWiktionary Definition lines:
    * #{{uncountable}} 'Abandon' is a state where you do not control yourself.
    * # Straying from the right or usual course; wandering.
    * # If a boat runs or goes 'ground', its bottom goes onto the ground
    * and it is not floating freely anymore. {{antonyms|afloat}}
    * #{{context|music}} , {{countable}} A 'keyboard' is a range of black and white
    * keys (buttons) on a musical instrument.
    * Returns: a 2-tuple containing to Seq[String]s. The first is the sequence of cleaned-up
    * definitions obtained from splitting up the raw definition line when it contains multiple
    * paraphrases, separated by semicolons. The second is the sequence of metadata strings, i.e.,
    * strings captured from the {{_}} tags embedded in the raw definition. These tend to have some
    * (loose) structure to them, for e.g., {{context|music}}, {{antonyms|afloat}}, {{math}},
    * {{past tense and participle of|attack}} etc. but currently we are just passing those strings
    * as is. When we get to actually using them, we will have to break this down into more useful form.
    * the challenge is in coming up with a meaningful representation that multiple different sources can
    * be normalized to.
    */
  def cleanUp(definitionRawLine: String): (Seq[String], Seq[String]) = {
    // Remove leading '#' character 
    val defPoundAtBeginningPattern = "^#".r
    val defBeginningPoundStripped: String = defPoundAtBeginningPattern replaceFirstIn (definitionRawLine, "")

    // Capture meta info - stuff in curly braces  if present in metaData seq - to be returned.
    val defMetaInfoPattern = """\{\{([^}]*)\}\}""".r
    val matches = defMetaInfoPattern findAllMatchIn (defBeginningPoundStripped)
    val metaData = (matches map { m => m.group(1) }).toSeq

    // Remove meta info - there could be a cluster of multiple of these
    // separated by semicolon or comma like the last e.g. in above documentation.
    val defMetaInfoClusterPattern = s"""(${defMetaInfoPattern})((\\s*[,;]\\s*${defMetaInfoPattern})*)""".r
    val defPoundMetaStripped: String = defMetaInfoClusterPattern replaceAllIn (defBeginningPoundStripped, "")

    // Remove all bracketed expressions
    val parenPattern = """\([^)]*\)""".r
    val defPoundMetaParenStripped: String = parenPattern replaceAllIn (defPoundMetaStripped, "")

    // Remove quotes from quoted words, like the definition term
    val defQuotedWordPattern = """(?<=\W|^)'([^']+)'(?=\W|$)""".r
    val defPoundMetaQuotesStripped = defQuotedWordPattern replaceAllIn (defPoundMetaParenStripped, m => m.group(1))

    // Break the line up into multiple definitions if separated by semicolons
    val multipleDefs = defPoundMetaQuotesStripped.split(";").toSeq map { x => x.trim }

    (multipleDefs, metaData)
  }
}
