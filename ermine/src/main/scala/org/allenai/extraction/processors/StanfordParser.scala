package org.allenai.extraction.processors

import org.allenai.extraction.FlatProcessor

import edu.stanford.nlp.pipeline.StanfordCoreNLP

import scala.io.Source

import java.io.Writer
import java.util.Properties

/** Wrapper around stanford parser, with static configs. Note that this takes a substantial (more
  * than 10 seconds) time to construct, and uses about 600MB of memory.
  */
object StanfordParser extends FlatProcessor {
  /** Lazily instantiated stanford pipeline. */
  lazy val pipeline = {
    // Construct a StanfordCoreNLP instance.
    val props: Properties = new Properties()
    props.setProperty("encoding", "utf8")
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    props.setProperty("outputFormat", "xml")
    new StanfordCoreNLP(props)
  }

  /** Outputs the Stanford parse as XML from the given source. */
  override def processText(source: Source, destination: Writer): Unit = {
    // Stanford requires we load the entire text to process in memory.
    val sourceText = source.getLines().mkString("\n")
    // Run the processing.
    val annotation = pipeline.process(sourceText)
    // Output the results.
    pipeline.xmlPrint(annotation, destination)
  }
}
