package org.allenai.extraction.extractors

import java.io.File
import java.io.FileWriter
import java.io.Writer

import scala.io.Source

import org.allenai.common.Resource
import org.allenai.extraction.FlatExtractor
import org.allenai.taggers.Cascade
import org.allenai.taggers.Cascade.LevelDefinition
import org.allenai.taggers.ChunkedTaggerApp
import org.allenai.taggers.Level
import org.allenai.taggers.tag.Tagger
import edu.knowitall.repr.sentence.Chunks
import edu.knowitall.repr.sentence.Lemmas
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.typer.Type

/** An extractor that processes definitions for a given class of terms. */
abstract class OpenRegexExtractor(cascadeFilePath: String)  extends FlatExtractor {

  /** A type alias for convenience since the Tagger API we are using deal with sentences 
   *  that are chunked and lemmatized */
  type Sentence = Tagger.Sentence with Chunks with Lemmas

  /** External NLP tools that are used to build the expected type from a sentence string. */
  lazy val chunker = new OpenNlpChunker()

  lazy val cascadeFile = new File(cascadeFilePath)
    
  // Load the Input Cascade file to get all Level Definitions
  lazy val (levelDefinitions, extractors) = Cascade.partialLoad(cascadeFile)
  lazy val levels: Seq[Level[Sentence]] =
      levelDefinitions.map { case LevelDefinition(title, text) => Level.fromString(title, text) }

  // Create the required Cascade object
  lazy val cascade = new Cascade[Sentence]("DefinitionExtractor", levels, extractors)

  // The ChunkedTaggerApp object is used to process the input raw sentence into a
  // chunked and lemmatized Sentence object to be passed to the 'extract' method
  lazy val app = new ChunkedTaggerApp(cascade)
    
  
  /** The main extraction method: takes an Input Source with the text to process and writes
   *  extraction output out to the specified Writer.  */
  override protected def extractInternal(input: Source, destination: Writer): Unit = {
    
    // Iterate over input sentences (definitions), processing each
    for (line <- input.getLines) {
        val results = extractText(line)

        // Output: First write out the input line.
        destination.write("INPUT:   " + line + "\n")
        
        // Then write out the extraction results.
        for (result <- results) {
          destination.write(result + "\n")
        }
        destination.write("\n")
    }
 
  }

  /** process : 
   *  - Run the Input  file through the extractor with the specified Cascade file and return
   *  Extractions as a sequence of strings */
  protected def process(allTypes: Seq[Type], lastLevelTypes: Seq[Type]) : Seq[String]
  
 
  protected def extractText(text: String) : Seq[String] = {
    // Create chunked and lematized Sentence
    val sentence = app.process(text)
        
    // Extract sentence to get all output types and the extractions based on rules specified in the
    // Cacasde file.
    val (allTypes, lastLevelTypes, extractions) = extract(cascade, sentence, levels)    
        
    // Call the process method on the specific extractor (abstract here)
    process(allTypes, lastLevelTypes)
  }
  
  
  /** TODO : This should be pulled out of this class and added as an overloaded 'extract' method to the 
   *  Extractor object under org.allenai.taggers. The 'extract' methods currently implemented
   *  do not return all Types in the return result, but only the top level Types. We want access
   *  to all Types.
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


