package org.allenai.extraction.processors.definition

import org.allenai.extraction.FlatProcessor

import java.io.Writer

import scala.io.Source

import spray.json.pimpAny

/** A bare-bones processor meant to expose the DefinitionCleanupUtility's functionality to web demo.
  * It takes some definition text (no word class or other information, just the main text of the
  * definition), and cleans it up based on requirements to pass to OpenRegex.
  * Uses the DefinitionCleanupUtility's cleanUp method. This is meant for the web demo to send
  * user-entered, possibly unclean definition input to have it cleaned-up/formatted as required.
  * @param wordClassOption and optional parameter for the word class of the definition to be
  * processed. Will default to noun.
  */
class DefinitionTextPreprocessor(wordClassOption: Option[String]) extends FlatProcessor {

  val wordClass = wordClassOption getOrElse ("noun")

  /** The main processing method: takes a possibly noisy definition from Input source and
    * returns a cleaned-up definition .
    */
  override def processText(input: Source, destination: Writer): Unit = {
    //Start output Json 
    destination.write("[\n")
    var beginning = true
    var defId = 1
    for (line <- input.getLines) {
      val cleanedUpDefs = DefinitionCleanupUtility.cleanUp(line)
      val preprocessedDefOp =
        PreprocessedDefinition(Some("User Input"), defId, line, "", Some(wordClass), cleanedUpDefs, Seq.empty[String])
      if (!beginning) {
        destination.write(",\n")
      }
      destination.write(preprocessedDefOp.toJson.compactPrint + "\n")
      beginning = false
      defId += 1
    }
  }
}