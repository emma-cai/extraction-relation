package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.rdf.{ DependencyGraph, TurtleProcessor }
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.SailGraph
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** Processor to map nominal nodes to events.
  * @param nominalizations the TTL file holding wordnet nominalizations
  */
class ExtractionDenominalize(nominalizations: Source) extends TurtleProcessor {
  // SPARQL query for nodes in rel: relation with denominalization != lemma
  val query =
    """CONSTRUCT { ?node wn:deriv ?verb ; rdfs:label ?verb . } WHERE {
      { ?node ?rel ?x . } UNION { ?x ?rel ?node . }
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
      ?node token:text ?text .
      ?wn rdfs:label ?text ; wn:deriv ?verb .
      FILTER NOT EXISTS { ?node token:lemma ?verb . }
    }"""

  override def processGraph(graph: SailGraph): Unit = {
    // input plus nominalization table
    val combinedGraph = new MemoryStoreSailGraph()
    DependencyGraph.setNamespaces(combinedGraph)
    DependencyGraph.fromTurtle(combinedGraph, nominalizations.reset())
    DependencyGraph.copy(graph, combinedGraph)

    // match patterns
    for (map <- DependencyGraph.executeSparql(combinedGraph, query)) {
      // add results
      graph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toUri)
    }

    combinedGraph.shutdown()
  }
}
