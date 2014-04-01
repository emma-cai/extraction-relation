package org.allenai.extraction.stanford

import org.allenai.extraction.Extractor

import edu.stanford.nlp.pipeline.StanfordCoreNLP

import scala.io.Source

import java.io.Writer
import java.util.Properties

/** Wrapper around stanford parser, with static configs. Note that this takes a very long time to
  * construct, and uses about 1.5G of memory.
  */
class StanfordParser extends Extractor {
  val pipeline = {
    // Construct a StanfordCoreNLP instance.
    val props: Properties = new Properties()
    props.setProperty("encoding", "utf8")
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    props.setProperty("outputFormat", "xml")
    new StanfordCoreNLP(props)
  }

  /** Extracts the Stanford parse as XML from the given source. */
  override def extract(source: Source, destination: Writer): Unit = {
    // Stanford requires we load the entire text to process in memory.
    val sourceText = source.getLines().mkString("\n")
    // Run the processing.
    val annotation = pipeline.process(sourceText);
    // Output the results.
    pipeline.xmlPrint(annotation, destination)
  }
}
