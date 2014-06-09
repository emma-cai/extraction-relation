package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.manager.io.SourceInputStream
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import java.io.Writer
import java.nio.charset.StandardCharsets
import org.apache.commons.io.output.WriterOutputStream


/** A demo extractor that reads and writes Turtle with no graph changes */
object TurtleProcessor extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    // convert input
    val source = sources(0)
    val sourceStream = new SourceInputStream(source, "UTF-8")
    // load graph
    val graph = new DependencyGraph()
    graph.loadRDF(sourceStream, "http://aristo.allenai.org/", "turtle", null)

    // convert output
    val sink: Writer = destinations(0)
    val sinkStream: WriterOutputStream = new WriterOutputStream(sink, StandardCharsets.UTF_8)
    // write graph
    graph.saveRDF(sinkStream, "turtle")
  }
}
