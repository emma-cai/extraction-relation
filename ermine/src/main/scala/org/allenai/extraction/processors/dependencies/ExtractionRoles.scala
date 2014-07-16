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
    def addArgs(node: Vertex): Unit = {
      // define input dependency to output role relations
      val roles: Seq[(String, String)] = Seq(
        ("nsubj", "pred:agent"),
        ("nsubjpass", "pred:object"),
        ("dobj", "pred:object"),
        ("iobj", "pred:arg"),
        ("advmod", "pred:arg"),
        ("tmod", "pred:arg"))
      for ((dep, role) <- roles) {
        addArg(node, dep, role)
      }
      addPreps(node)
      addRelClause(node)
    }

    /** query for arg values and add to outputGraph */
    def addArg(node: Vertex, dep: String, role: String) = {
      val uri: String = node.toUri
      // add filter to exclude that/which/etc as rcmod subject
      // TODO: declare filters in dep-role table?
      val extraFilter: String = dep match {
        case "nsubj" => s"""FILTER NOT EXISTS { ?rcmod dep:rcmod <$uri> .
                                                ?nsubj token:pos "WDT" . }"""
        case _ => "" // no filter
      }
      val query: String = s"""
        # find dep relation
        SELECT ?$dep WHERE {
          <$uri> dep:$dep ?$dep .
          FILTER NOT EXISTS { <$uri> dep:cop ?cop . }
          $extraFilter
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

    /** query for relative clauses add to outputGraph */
    def addRelClause(node: Vertex) = {
      val uri: String = node.toUri
      val query: String = s"""
        # find rcmod head
        SELECT ?rcmod WHERE {
          <$uri> dep:rcmod ?rcmod .
        }"""
      val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(graph, query)
      for (map <- result) {
        val rcmod: Vertex = map("rcmod")
        val rcmodUri: String = rcmod.toUri
        addArgs(rcmod)
        // find first missing pred
        Seq("pred:agent", "pred:object", "pred:arg").find { role =>
          val predQuery: String = s"""
            SELECT ?node WHERE {
              <$rcmodUri> $role ?node .
            }"""
          val predResult: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(graph, predQuery)
          if (predResult.isEmpty) {
            graph.addEdge(rcmod, rcmod, node, role)
            // Found, stop
            true
          } else {
            // Not found, continue
            false
          }
        }
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
