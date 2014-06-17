package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to map nominal nodes to events */
object ExtractionDenominalize extends TextProcessor {
  override val numInputs = 2
  override val numOutputs = 2

  val inputGraph = new DependencyGraph() // unmodified input
  val combinedGraph = new DependencyGraph() // input plus nominalization table
  val outputGraph = new DependencyGraph()

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
    inputGraph.loadTurtle(source)

    val nominalizations = sources(1)
    combinedGraph.loadTurtle(source.reset()) // re-read input
    combinedGraph.loadTurtle(nominalizations)

    // match patterns
    for (map <- combinedGraph.executeQuery(query)) {
      println(map)
      // add results
      outputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toLiteral)
    }

    val sink: Writer = destinations(0)
    inputGraph.saveTurtle(sink)
    outputGraph.saveTurtle(sink)

    val debugSink: Writer = destinations(1)
    outputGraph.saveTurtle(debugSink)

    inputGraph.shutdown()
    combinedGraph.shutdown()
    outputGraph.shutdown()
  }

}
