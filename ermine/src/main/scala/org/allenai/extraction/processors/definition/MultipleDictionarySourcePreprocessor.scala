package org.allenai.extraction.processors.definition

import org.allenai.extraction.FlatProcessor

import java.io.Writer

import scala.io.Source

import spray.json.pimpAny

/** Preprocessor that takes scraped text from multiple dictionary sources as input and converts it
  * to the format expected by the Definition Extractor, which is defined in the PreprocessedDefinition
  * class.
  * @param wordClasses the set of wordclasses to consider. If this set is specified, i.e., non-empty, definitions
  * of terms belonging to any class outside of this set will NOT be processed/written out.
  * Default: Empty set, which means there are no filters, so the preprocessor will process/write out
  * all definitions of all word classes.
  * @param definitionSources the dictionary sources to consider as the input resource has definitions from various
  * sources - Wiktionary, Wordnet, AHD, Century etc.
  * If this set is specified, i.e., non-empty, definitions from sources outside of this set will NOT be
  * processed/written out.
  * Default: Empty set, which means there are no filters, so the preprocessor will process/write out
  * definitions from all sources.
  */
class MultipleDictionarySourcePreprocessor(
    wordClasses: Set[String] = Set.empty,
    definitionSources: Set[String] = Set.empty) extends FlatProcessor {
  val wordClassesLowerCase = wordClasses.map(x => x.toLowerCase)
  val definitionSourcesLowerCase = definitionSources.map(x => x.toLowerCase)

  /** The main extraction method: takes an Input Source with the scraped text from multiple dictionary
    * sources in the format resembling:
    * "<term>real</term><_pos>noun</_pos><_source>WIKTIONARY</_source>real : A coin worth one real."
    * and writes extraction output in the format defined by the PreprocessedDefinition class.
    */
  override def processText(input: Source, destination: Writer): Unit = {
    //Start output Json 
    destination.write("[\n")
    var beginning = true
    var defId = 1
    for {
      line <- input.getLines
      (term, termWordClass, termDefinition, definitionSource) <- breakLine(line.trim)
      if ((wordClassesLowerCase.contains(termWordClass.toLowerCase) || (wordClassesLowerCase.size == 0))
        && (definitionSourcesLowerCase.contains(definitionSource.toLowerCase) || (definitionSourcesLowerCase.size == 0)))
    } {
      val defAlts = cleanUp(termDefinition.stripPrefix("\"").stripSuffix("\"").trim)
      val preprocessedDefOp =
        PreprocessedDefinition(Some(definitionSource), defId, termDefinition, term, Some(termWordClass), defAlts, Seq.empty[String])
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
    * The scraped text from the various dictionaries, consists of one definition per line in a txt file,
    * with the format of <SomeRandomLookingNumberWeDon'tNeed>\t<PseudoXmlDefinition>.
    * Here's an e.g. of the PseudoXmlDefinition:
    * "<term>report</term><_pos>noun</_pos><_source>century</_source>report : Repute; public character."
    * Here's an e.g.of an entire line:
    * 0709.06429.017.1.1	"<term>report</term><_pos>noun</_pos><_source>century</_source>report : Repute; public character."
    */
  def breakLine(defnInputLine: String): Option[(String, String, String, String)] = {
    defnInputLine.split("\t") match {
      case Array(id, definitionXml, _*) => breakPseudoXml(definitionXml.trim)
      case _ => None
    }
  }

  /** breakPseudoXml : Break the input definition in pseudo-Xml into its constituent parts.
    * The input string will have a format similar to the below-
    * "<term>report</term><_pos>noun</_pos><_source>century</_source>report : Repute; public character."
    * This should return a result that is (an optional) tuple containing these values:
    * (<term>, <wordClass>, <definition>, <source>)
    * For the above e.g. xml line this would be:
    * ("report", "noun", "report : Repute; public character.", "century")
    */
  def breakPseudoXml(definitionXml: String): Option[(String, String, String, String)] = {
    // Patterns for term, word class and source
    val termPattern = """<term>(.+)</term>""".r
    val wordClassPattern = """<_pos>(.+)</_pos>""".r
    val sourcePattern = """<_source>(.+)</_source>""".r

    // Capture, term, word class and source respectively.
    val termOption = termPattern findFirstMatchIn (definitionXml) map { m => m.group(1) }
    val wordClassOption = wordClassPattern findFirstMatchIn (definitionXml) map { m => m.group(1) }
    val sourceOption = sourcePattern findFirstMatchIn (definitionXml) map { m => m.group(1) }

    // Progressively reduce the input string to just the text outside any of the tags, 
    // which represents the actual definition.
    var trimmedDefLine = termPattern replaceAllIn (definitionXml, "")
    trimmedDefLine = wordClassPattern replaceAllIn (trimmedDefLine, "")
    trimmedDefLine = sourcePattern replaceAllIn (trimmedDefLine, "")

    (termOption, wordClassOption, trimmedDefLine, sourceOption) match {
      case (Some(term), Some(wordClass), defn, Some(source)) if ((term.trim.length > 0) && (wordClass.trim.length > 0) && (defn.trim.length > 0) && (source.trim.length > 0)) =>
        Some((term.trim, wordClass.trim, defn.trim, source.trim))
      case _ => None
    }
  }

  /** cleanUp : Break the input definition into its constituent parts.
    * Cleaning up the input line will involve standard cleanUp from the DefinitionCleanuputility.
    * Further, the output from the DefinitionCleanupUtility will be broken down into multiple
    * alternate definitions where they exist, i.e., separated by semicolons, for e.g.,
    * the input definition "Repute; public character."  will be split and a seq of strings:
    * ("Repute", "public character") will be produced/returned.
    */
  def cleanUp(rawDefinition: String): Seq[String] = {
    val defCleanedUp = DefinitionCleanupUtility.cleanUp(rawDefinition)

    // Break the line up into multiple definitions if separated by semicolons
    defCleanedUp.split(";").toSeq map { x => x.trim }
  }
}