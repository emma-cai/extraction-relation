package org.allenai.extraction.processors

import java.io.File
import java.io.Writer

import scala.io.Source

import org.allenai.extraction.FlatProcessor
import org.allenai.taggers.Cascade
import org.allenai.taggers.Cascade.LevelDefinition
import org.allenai.taggers.ChunkedTaggerApp
import org.allenai.taggers.Level
import org.allenai.taggers.tag.Tagger

import edu.knowitall.repr.sentence.Chunks
import edu.knowitall.repr.sentence.Lemmas
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.typer.Type

/** An extractor that processes input text with OpenRegex with the specified Cascade file.
  * Given a cascade file parameter, this class will call OpenRegex API with that cascade file
  * to process input source and generate extractions- written to the specified destination.
  * @param [T] the output extraction type depending on the specific extractor
  * @param cascadeFilePath path to the Cascade file to be used by OpenRegex to perform extractions
  * on the input text.
  */
abstract class OpenRegexExtractor[T](cascadeFilePath: String) extends FlatProcessor {

  /** A type alias for convenience since the Tagger API we are using deal with sentences
    * that are chunked and lemmatized.
    */
  type Sentence = Tagger.Sentence with Chunks with Lemmas

  /** Create the file object from the Cascade File Path.
    */
  val cascadeFile = new File(cascadeFilePath)

  /** Load the Input Cascade file to get all Level Definitions.
    */
  val (levelDefinitions, extractors) = Cascade.partialLoad(cascadeFile)
  val levels: Seq[Level[Sentence]] =
    levelDefinitions.map { case LevelDefinition(title, text) => Level.fromString(title, text) }

  /** Create the required Cascade object.
    */
  val cascade = new Cascade[Sentence]("DefinitionExtractor", levels, extractors)

  /** The ChunkedTaggerApp object is used to process the input raw sentence into a
    * chunked and lemmatized Sentence object to be passed to the 'extract' method.
    */
  val app = new ChunkedTaggerApp(cascade)

  /** The main extraction method: takes an Input Source with the text to process and writes
    * extraction output out to the specified Writer.
    */
  override def processText(input: Source, destination: Writer): Unit = {
    // Iterate over input sentences (definitions), processing each
    for (line <- input.getLines) {
      val (results, tokens) = extractText(line)

      // Output: First write out the input line.
      destination.write("INPUT:   " + line + "\n")

      // Then write out the extraction results.
      for (result <- results) {
        destination.write(result + "\n")
      }
      destination.write("\n")
    }
  }

  /** process
    * Run the Input file through the extractor with the specified Cascade file and return
    * Extractions as a sequence of strings
    */
  protected def process(allTypes: Seq[Type], lastLevelTypes: Seq[Type], tokens: Seq[Lemmatized[ChunkedToken]]): Seq[T]

  /** extractText: Take in input text and output a sequence of extractions as strings formatted as required
    */
  protected def extractText(text: String): (Seq[T], Seq[Lemmatized[ChunkedToken]]) = {
    // Create chunked and lematized Sentence
    val sentence = app.process(text)

    // Extract sentence to get all output types and the extractions based on rules specified in the
    // Cascade file.
    val (allTypes, lastLevelTypes, extractions) = extract(cascade, sentence, levels)

    // Call the process method on the specific extractor (abstract here)
    (process(allTypes, lastLevelTypes, sentence.lemmatizedTokens), sentence.lemmatizedTokens)
  }

  /** TODO : This should be pulled out of this class and added as an overloaded 'extract' method to the
    * Extractor object under org.allenai.taggers. The 'extract' methods currently implemented
    * do not return all Types in the return result, but only the top level Types. We want access
    * to all Types.
    */
  def extract(cascade: Cascade[Sentence], sentence: Sentence, levels: Seq[Level[Sentence]]): (Seq[Type], Seq[Type], Seq[String]) = {
    var previousTypes = Seq.empty[Type]
    var previousLevelTypes = Seq.empty[Type]
    for (level <- levels) {
      val levelTypes = level.apply(sentence, previousTypes)

      previousTypes ++= levelTypes
      previousLevelTypes = levelTypes
    }
    (previousTypes, previousLevelTypes, cascade.extract(previousTypes).values.flatten.toSeq)
  }
}
