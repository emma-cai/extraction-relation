package org.allenai.extraction.processors.definition

import org.allenai.common.testkit.UnitSpec

/** A Test for the SimpleWiktionaryDefinitionPreprocessor class. Currently only implements unit tests
  * for the cleanUp method which uses a number of regexes to clean up raw definition text
  * from SimpleWiktionary.
  */
class SimpleWiktionaryDefinitionPreprocessorTest extends UnitSpec {

  val testPreprocessor = new SimpleWiktionaryDefinitionPreprocessor()

  "SimpleWiktionaryDefinitionPreprocessor's cleanUp Method" should "strip the leading '#' and return trimmed string" in {
    assert(testPreprocessor.cleanUp("# On the other side.") === (Seq[String]("On the other side."), Seq.empty[String]))
  }

  it should "strip out single quotes from quoted a term/string in a definition with a single quoted term/string" in {
    assert(testPreprocessor.cleanUp("An 'acre' is an imperial measure of area.")
      === (Seq[String]("An acre is an imperial measure of area."), Seq.empty[String]))
  }

  it should "strip out single quotes from quoted terms/strings in a definition with multiple quoted terms/strings" in {
    assert(testPreprocessor.cleanUp("An 'addict' is someone who is 'addicted to' something.")
      === (Seq[String]("An addict is someone who is addicted to something."), Seq.empty[String]))
  }

  it should "not strip single quotes that don't have a matching close quote" in {
    assert(testPreprocessor.cleanUp("A chess piece, often in the shape of a horse's head.")
      === (Seq[String]("A chess piece, often in the shape of a horse's head."), Seq.empty[String]))
  }

  it should "return the right result for a definition with a single meta info block" in {
    assert(testPreprocessor.cleanUp("{{countable}} An 'act' is a law made by the government.")
      === (Seq[String]("An act is a law made by the government."), Seq[String]("countable")))
  }

  it should "return the right result for a definition with meta info elsewhere in the definition than at the beginning" in {
    assert(testPreprocessor.cleanUp("# A 'knick-knack' is a small item or ornament that is not worth much. {{synonyms|tchotchke|trinket}}")
      === (Seq[String]("A knick-knack is a small item or ornament that is not worth much."), Seq[String]("synonyms|tchotchke|trinket")))
  }

  it should "return the right result for a definition with multiple meta info blocks" in {
    assert(testPreprocessor.cleanUp("{{UK}} {{uncountable}} 'Amber' is the middle light on traffic lights.")
      === (Seq[String]("Amber is the middle light on traffic lights."), Seq[String]("UK", "uncountable")))
  }

  it should "return the right result for a definition with a single parenthesized expression" in {
    assert(testPreprocessor.cleanUp("In a style typical of the Americas (North and South America).")
      === (Seq[String]("In a style typical of the Americas ."), Seq.empty[String]))
  }

  it should "return the right result for a definition with multiple parenthesized expressions" in {
    assert(testPreprocessor.cleanUp("('numbers') Close to ()")
      === (Seq[String]("Close to"), Seq.empty[String]))
  }

  it should "return the right result when parenthesized expressions are embedded in curly-braced expressions" in {
    assert(testPreprocessor.cleanUp("{{(transitive) verb}} If one thing 'accompanies' something else, it comes or goes with it, or they happen together.")
      === (Seq[String]("If one thing accompanies something else, it comes or goes with it, or they happen together."),
        Seq[String]("(transitive) verb")))
  }

  it should "return the right result when single-quoted expressions are embedded in curly-braced expressions" in {
    assert(testPreprocessor.cleanUp("{{'transitive' verb}} If one thing 'accompanies' something else, it comes or goes with it, or they happen together.")
      === (Seq[String]("If one thing accompanies something else, it comes or goes with it, or they happen together."),
        Seq[String]("'transitive' verb")))
  }

  it should "return the right result when curly-braced expressions are embedded in parenthesized expressions" in {
    assert(testPreprocessor.cleanUp("'Jesus' ({{IPA|/he'sus/}}) is a boy's name, especially in Spanish-speaking counties.")
      === (Seq[String]("Jesus  is a boy's name, especially in Spanish-speaking counties."),
        Seq[String]("IPA|/he'sus/")))
    assert(testPreprocessor.cleanUp("({{P+NP}}, 'after the noun' &amp; {{P-comp}}) If A is 'above' B, A is higher than or before B, but not touching B.")
      === (Seq[String]("If A is above B, A is higher than or before B, but not touching B."),
        Seq[String]("P+NP", "P-comp")))
  }

  it should "return the right result when there is a cluster of meta infos (curly-braced expressions) separated by semi-colons or commas" in {
    assert(testPreprocessor.cleanUp("#{{context}},{{music}} , {{countable}}	 A 'keyboard' is a range of black and white keys (buttons) on a musical instrument.")
      === (Seq[String]("A keyboard is a range of black and white keys  on a musical instrument."),
        Seq[String]("context", "music", "countable")))
    assert(testPreprocessor.cleanUp("#{{context|music}} ; {{countable}} A 'keyboard' is a range of black and white keys (buttons) on a musical instrument.")
      === (Seq[String]("A keyboard is a range of black and white keys  on a musical instrument."),
        Seq[String]("context|music", "countable")))
  }

  it should "split the definition into multiple parts when ';' is present" in {
    assert(testPreprocessor.cleanUp("Straying from the right or usual course; wandering.")
      === (Seq[String]("Straying from the right or usual course", "wandering."),
        Seq.empty[String]))
  }

  it should "return a Seq with a single empty string if there is no definition (just meta info)" in {
    assert(testPreprocessor.cleanUp("#{{plural of|khaki}}")
      === (Seq[String](""), Seq[String]("plural of|khaki")))
  }

  it should "remove any empty (single/double) quoted strings that appear in the definition" in {
    assert(testPreprocessor.cleanUp("The Indus Valley bullfrog '' is a large frog found in Pakistan and India.")
      === (Seq[String]("The Indus Valley bullfrog  is a large frog found in Pakistan and India."), Seq.empty[String]))
  }
}