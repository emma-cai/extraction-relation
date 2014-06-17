package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph

import scala.io.Source

import java.io.Writer

/** A demo extractor that reads and writes Turtle with no graph changes */
object TurtleProcessor extends TextProcessor {
  override val numInputs = 6
  override val numOutputs = 1

  val inputGraph = new DependencyGraph()
  val outputGraph = new DependencyGraph()

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val graph = new DependencyGraph()
    for (source <- sources) {
      inputGraph.loadTurtle(source)
    }

    val sink: Writer = destinations(0)
    inputGraph.saveTurtle(sink)

    inputGraph.shutdown()
    outputGraph.shutdown()
  }
}
