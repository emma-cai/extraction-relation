package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to map nominal nodes to events */
object ExtractionDenominalize extends TextProcessor {
  override val numInputs = 4
  override val numOutputs = 1

  val inputGraph = new DependencyGraph()
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
    for (source <- sources) {
      inputGraph.loadTurtle(source)
    }

    // match patterns
    for (map <- inputGraph.executeQuery(query)) {
      // add results
      outputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toLiteral)
    }

    val sink: Writer = destinations(0)
    outputGraph.saveTurtle(sink)

    inputGraph.shutdown()
    outputGraph.shutdown()
  }

}
