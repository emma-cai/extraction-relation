package org.allenai.extraction.processors

import java.io.{ File, Writer }

import scala.io.Source

/** An extractor that processes definitions for a given class of terms using OpenRegex. 
 *  A directory is expected per word class, with the cascade file in it being called defn.cascade.
 *  wordClass can take different values like Noun, Adjective, Verb, Expression etc.
 *  Definitions have different formats depending on the word class of the term being defined. This will
 *  impact the rules defined in Openregex, so we will have a set of rules and a cascade file for each word class. 
 *  @param dataPath  Path of the data directory that will contain OpenRegex rule files to be used for the definition extraction. 
 *  @param wordClass The word class, for e.g., noun/verb/adjective to be processed. A subdirectory is expected under the specified dataPath,
 *                   for each word class. So the specified wordClass here is appended to the dataPath to get to the necessary rule files. */
abstract class DefinitionOpenRegexExtractor(dataPath: String, val wordClass: String) extends OpenRegexExtractor(dataPath + "//" + wordClass + "//defn.cascade") {

  /** The main extraction method: Input Source contains a bunch of definitions preprocessed
    * into the format: <term>\t<wordClass>\t<definition>. Output will be written out to the
    * specified destination
    */
  override protected def processInternal(defnInputSource: Source, destination: Writer): Unit = {

    // Iterate over input sentences (definitions), preprocess each and send it to the 
    for (line <- defnInputSource.getLines) {
      val (term, termWordClass, termDefinition) = preprocessLine(line)
      if (termWordClass.equalsIgnoreCase(wordClass)) {
        val results = super.extractText(termDefinition)

        // Output: First write out the input line.
        destination.write("DEFINITION:   " + line + "\n")

        // Then write out the extraction results.
        for (result <- results) {
          destination.write(result + "\n")
        }
        destination.write("\n")
      }
    }

  }

  /** prerocessLine : Break the input line into its constituent parts.
    * The assumption here is that the required preprocessor for the given Definition corpus
    * has been run earlier in the pipeline so that what feeds into this extractor (DefnExtractor)
    * is of the form: <Term>\t<WordClass>\t<Definition> per line.
    */
  def preprocessLine(defnInputLine: String): (String, String, String) = {
    defnInputLine.split("\t") match {
      case Array(term, termWordClass, termDefinition, _*) => (term, termWordClass, termDefinition)
      case _ => ("", "", "")
    }
  }
}

