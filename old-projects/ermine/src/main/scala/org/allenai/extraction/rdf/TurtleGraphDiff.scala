package org.allenai.extraction.rdf

import org.allenai.extraction.TextProcessor

import com.tinkerpop.blueprints.{ Direction, Edge }
import com.tinkerpop.blueprints.impls.sail.SailGraph
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.collection.JavaConverters._
import scala.io.Source

import java.io.Writer

/** A TTL processor that outputs the difference of two graphs. The input should be two TTL files,
  * and the output will be a TTL file containing all of the new edges added to the graph. Removals
  * are not noted.
  */
object TurtleGraphDiff extends TextProcessor {
  override val numInputs = 2
  override val numOutputs = 1

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val startGraph = new MemoryStoreSailGraph()
    DependencyGraph.fromTurtle(startGraph, sources(0))
    val endGraph = new MemoryStoreSailGraph()
    DependencyGraph.fromTurtle(endGraph, sources(1))
    val diffGraph = new MemoryStoreSailGraph()

    val startEdges = Set.empty[Edge] ++ startGraph.getEdges.asScala
    val endEdges = Set.empty[Edge] ++ endGraph.getEdges.asScala
    val newEdges = endEdges.diff(startEdges)
    for (edge <- newEdges) {
      diffGraph.addEdge(edge.getId, edge.getVertex(Direction.OUT), edge.getVertex(Direction.IN),
        edge.getLabel())
    }

    DependencyGraph.toTurtle(diffGraph, destinations(0))
    startGraph.shutdown()
    endGraph.shutdown()
    diffGraph.shutdown()
  }
}
