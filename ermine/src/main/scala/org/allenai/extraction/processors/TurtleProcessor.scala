package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import java.io.Writer

/** A demo extractor that reads and writes Turtle with no graph changes */
object TurtleProcessor extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  val inputGraph = new MemoryStoreSailGraph()
  DependencyGraph.setNamespaces(inputGraph)

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    DependencyGraph.fromTurtle(inputGraph, source)

    val sink: Writer = destinations(0)
    DependencyGraph.toTurtle(inputGraph, sink)

    inputGraph.shutdown()
  }
}
