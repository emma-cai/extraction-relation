package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to add labels and string descriptions to extracted nodes */
object ExtractionRoles extends TextProcessor {
  override val numInputs = 4
  override val numOutputs = 1

  val inputGraph = new DependencyGraph()
  val outputGraph = new DependencyGraph()

  // SPARQL query for denominalized nodes
  val denomQuery: String =
    """CONSTRUCT { ?node pred:object ?obj . } WHERE {
      ?node wn:deriv ?verb ; dep:prep_of ?obj .
    }"""

  // SPARQL query for nodes with added rel: relation
  val relQuery: String =
    """SELECT ?node WHERE {
      { ?node ?rel ?x . } UNION { ?x ?rel ?node . }
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
    }"""

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    for (source <- sources) {
      inputGraph.loadTurtle(source)
    }

    // match patterns
    for (map <- inputGraph.executeQuery(denomQuery)) {
      // add to input graph for next query
      inputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toLiteral)
      // add to output graph for output
      outputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toLiteral)
    }

    for (map <- inputGraph.executeQuery(relQuery)) {
      addArgs(map("node"))
    }

    val sink: Writer = destinations(0)
    outputGraph.saveTurtle(sink)

    inputGraph.shutdown()
    outputGraph.shutdown()
  }

  /** map input dependencies to output args */
  def addArgs(node: Vertex) = {
    // define input dependency to output role relations
    val roles: Seq[Tuple2[String, String]] = Seq(
      ("nsubj", "pred:agent"),
      ("dobj", "pred:object"),
      ("iobj", "pred:arg"),
      ("tmod", "pred:arg"))
    for ((dep, role) <- roles) {
      addArg(node, dep, role)
    }
    addPreps(node)
  }

  /** query for arg values and add to outputGraph */
  def addArg(node: Vertex, dep: String, role: String) = {
    val uri: String = node.toUri
    val query: String = s"""
      # find dep relation
      SELECT ?$dep WHERE {
        <$uri> dep:$dep ?$dep .
      }"""
    val result: Seq[Map[String, Vertex]] = inputGraph.executeQuery(query)
    for (map <- result) {
      val obj: Vertex = map(dep)
      outputGraph.addEdge(null, node, obj, role)
    }
  }

  /** query for PP values and add to outputGraph */
  def addPreps(node: Vertex) = {
    val uri: String = node.toUri
    val query: String = s"""
      # find prep_* relation and extract the preposition substring
      SELECT ?prep ?obj WHERE {
        <$uri> ?dep ?obj .
        FILTER NOT EXISTS { <$uri> pred:object ?obj . } # from wn:deriv
        FILTER(CONTAINS(str(?dep), "/dep/prep_")) .
        BIND(STRAFTER(str(?dep), "_") AS ?prep) .
      }"""
    val result: Seq[Map[String, Vertex]] = inputGraph.executeQuery(query)
    for (map <- result) {
      // use preposition as new predicate
      val prep: String = map("prep").toLiteral
      val obj: Vertex = map("obj")
      outputGraph.addEdge(null, node, obj, s"pred:$prep")
    }
  }

}
