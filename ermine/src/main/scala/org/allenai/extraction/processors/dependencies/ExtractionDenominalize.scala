package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to map nominal nodes to events */
object ExtractionDenominalize extends TextProcessor {
  override val numInputs = 2
  override val numOutputs = 2

  val inputGraph = new MemoryStoreSailGraph() // unmodified input
  DependencyGraph.setNamespaces(inputGraph)
  val combinedGraph = new MemoryStoreSailGraph() // input plus nominalization table
  DependencyGraph.setNamespaces(combinedGraph)
  val outputGraph = new MemoryStoreSailGraph()
  DependencyGraph.setNamespaces(outputGraph)

  // SPARQL query for nodes in rel: relation with denominalization != lemma
  val query =
    """CONSTRUCT { ?node wn:deriv ?verb ; rdfs:label ?verb . } WHERE {
      { ?node ?rel ?x . } UNION { ?x ?rel ?node . }
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
      ?node token:text ?text .
      ?wn rdfs:label ?text ; wn:deriv ?verb .
      FILTER NOT EXISTS { ?node token:lemma ?verb . }
    }"""

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    DependencyGraph.fromTurtle(inputGraph, source)

    val nominalizations = sources(1)
    DependencyGraph.fromTurtle(combinedGraph, source.reset()) // re-read input
    DependencyGraph.fromTurtle(combinedGraph, nominalizations)

    // match patterns
    for (map <- DependencyGraph.executeSparql(combinedGraph, query)) {
      // add results
      outputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toUri)
    }

    val sink: Writer = destinations(0)
    DependencyGraph.toTurtle(inputGraph, sink)
    DependencyGraph.toTurtle(outputGraph, sink)

    val debugSink: Writer = destinations(1)
    DependencyGraph.toTurtle(outputGraph, debugSink)

    inputGraph.shutdown()
    combinedGraph.shutdown()
    outputGraph.shutdown()
  }

}
