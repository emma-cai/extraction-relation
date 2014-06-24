package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.rdf.{ DependencyGraph, TurtleProcessor }
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.{ Edge, Vertex }
import com.tinkerpop.blueprints.impls.sail.SailGraph

/** processor to add labels and string descriptions to extracted nodes */
object ExtractionRoles extends TurtleProcessor {
  override def processGraph(graph: SailGraph): Unit = {
    new InternalProcessor(graph).process()
  }

  /** Helper class, holding a reference to the IO graph. */
  class InternalProcessor(graph: SailGraph) {
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

    /** map input dependencies to output args */
    def addArgs(node: Vertex) = {
      // define input dependency to output role relations
      val roles: Seq[(String, String)] = Seq(
        ("nsubj", "pred:agent"),
        ("dobj", "pred:object"),
        ("iobj", "pred:arg"),
        ("tmod", "pred:arg"))
      for ((dep, role) <- roles) {
        addArg(node, dep, role)
      }
      addPreps(node)
    }

    /** query for arg values and add to graph */
    def addArg(node: Vertex, dep: String, role: String) = {
      val uri: String = node.toUri
      val query: String = s"""
        # find dep relation
        SELECT ?$dep WHERE {
          <$uri> dep:$dep ?$dep .
        }"""
      val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(graph, query)
      for (map <- result) {
        val obj: Vertex = map(dep)
        graph.addEdge(null, node, obj, role)
      }
    }

    /** query for PP values and add to graph */
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
      val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(graph, query)
      for (map <- result) {
        // use preposition as new predicate
        val prep: String = map("prep").toStringLiteral
        val obj: Vertex = map("obj")
        graph.addEdge(null, node, obj, s"pred:$prep")
      }
    }

    def process(): Unit = {
      // match patterns
      for (map <- DependencyGraph.executeSparql(graph, denomQuery)) {
        // add to graph for next query
        graph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toUri)
      }

      for (map <- DependencyGraph.executeSparql(graph, relQuery)) {
        addArgs(map("node"))
      }
    }
  }
}
