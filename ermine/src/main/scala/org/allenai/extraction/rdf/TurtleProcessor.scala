package org.allenai.extraction.rdf

import org.allenai.extraction.FlatProcessor

import com.tinkerpop.blueprints.impls.sail.SailGraph
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import java.io.Writer

/** Turtle processor. Takes as input a turtle file, which is loaded and passed to `processGraph`.
  */
abstract class TurtleProcessor extends FlatProcessor {
  override def processText(source: Source, destination: Writer): Unit = {
    val graph = new MemoryStoreSailGraph()
    DependencyGraph.fromTurtle(graph, source)
    DependencyGraph.setNamespaces(graph)

    processGraph(graph)

    DependencyGraph.toTurtle(graph, destination)
    graph.shutdown()
  }

  def processGraph(graph: SailGraph): Unit
}
