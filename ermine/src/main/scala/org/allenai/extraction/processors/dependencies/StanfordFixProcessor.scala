package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.rdf.{ DependencyGraph, TurtleProcessor }
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.SailGraph

/** processor to fix common dependency-parse errors */
object StanfordFixProcessor extends TurtleProcessor {
  // SPARQL queries
  val queries: Seq[String] = Seq(
    // broken PP: X prep Y, Y dep Z -> X prepc_Y Z
    """CONSTRUCT { ?x ?p ?z . } WHERE {
      ?x dep:prep ?y .
      ?y dep:dep ?z .
      ?y token:pos "IN" .
      ?z token:pos ?pos .
      FILTER(STRSTARTS(str(?pos), "VB")) .
      ?y token:lemma ?prep .
      BIND(CONCAT("dep:prepc_", str(?prep)) AS ?p)
    }""",
    // broken PP: X prep Y, Y dep Z -> X prep_Y Z
    """CONSTRUCT { ?x ?p ?z . } WHERE {
      ?x dep:prep ?y .
      ?y dep:dep ?z .
      ?y token:pos "IN" .
      ?z token:pos ?pos .
      FILTER(!STRSTARTS(str(?pos), "VB")) .
      ?y token:lemma ?prep .
      BIND(CONCAT("dep:prep_", str(?prep)) AS ?p)
    }""",
    // reattach temporal PP on dobj to verb
    """CONSTRUCT { ?x ?prep ?z . } WHERE {
      ?x dep:dobj ?y .
      ?y basic:prep ?p .
      ?p basic:pobj ?z .
      ?y ?prep ?z . # non-basic dep:prep_?p
      { ?z ne:type "DATE" . }
      UNION
      { ?z ne:type "TIME" . }
    }""",
    // use for-PP on dobj as nsubj of xcomp
    """CONSTRUCT { ?c dep:nsubj ?z . } WHERE {
      ?x dep:dobj ?y .
      ?y dep:prep_for ?z .
      ?x dep:xcomp ?c .
      FILTER NOT EXISTS { ?c dep:nsubj ?s . }
    }""",
    // move non-temporal tmod to dobj
    """CONSTRUCT { ?x dep:dobj ?y . } WHERE {
      ?x dep:tmod ?y .
      FILTER NOT EXISTS { ?y ne:type "DATE" . }
      FILTER NOT EXISTS { ?y ne:type "TIME" . }
      FILTER NOT EXISTS { ?x dep:dobj ?z . }
    }""")

  override def processGraph(graph: SailGraph): Unit = {
    // match patterns
    for {
      query <- queries
      map <- DependencyGraph.executeSparql(graph, query)
      head = map("subject")
      tail = map("object")
      edge = map("predicate")
    } {
      // add results
      graph.addEdge(edge, head, tail, edge.toIdString)
    }
  }
}
