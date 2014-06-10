package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.manager.io.SourceInputStream
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import java.io.Writer
import java.nio.charset.StandardCharsets
import org.apache.commons.io.output.WriterOutputStream


/** processor to fix common dependency-parse errors */
object StanfordFixProcessor extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  // SPARQL queries
  val queries: Seq[String] = Seq(
    // broken PP: X prep Y, Y dep Z -> X prepc_Y Z
    """CONSTRUCT { ?x ?p ?z . } WHERE {
      ?x dep:prep ?y .
      ?y dep:dep ?z .
      ?y token:pos "IN" .
      ?z token:pos ?pos .
      FILTER(regex(str(?pos), "^VB")) .
      ?y token:lemma ?prep .
      BIND(CONCAT("dep:prepc_", str(?prep)) AS ?p)
    }""",
    // broken PP: X prep Y, Y dep Z -> X prep_Y Z
    """CONSTRUCT { ?x ?p ?z . } WHERE {
      ?x dep:prep ?y .
      ?y dep:dep ?z .
      ?y token:pos "IN" .
      ?z token:pos ?pos .
      FILTER(!regex(str(?pos), "^VB")) .
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
    }"""
  )

  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    // convert input
    val source = sources(0)
    val sourceStream = new SourceInputStream(source, "UTF-8")
    // load graph
    val graph = new DependencyGraph()
    graph.loadRDF(sourceStream, "http://aristo.allenai.org/", "turtle", null)

    // match patterns
    for (query <- queries; map <- graph.executeQuery(query)) {
      // add results
      graph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toLiteral)
    }

    // convert output
    val sink: Writer = destinations(0)
    val sinkStream: WriterOutputStream = new WriterOutputStream(sink, StandardCharsets.UTF_8)
    // write graph
    graph.saveRDF(sinkStream, "turtle") // original input
    graph.outputGraph.saveRDF(sinkStream, "turtle") // any additions

    // close
    graph.shutdown()
  }
}
