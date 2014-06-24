package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import util.control.Breaks._
import scala.io.Source

import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to add labels and string descriptions to extracted nodes */
object ExtractionRoles extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 2

  val inputGraph = new MemoryStoreSailGraph()
  DependencyGraph.setNamespaces(inputGraph)
  val outputGraph = new MemoryStoreSailGraph()
  DependencyGraph.setNamespaces(outputGraph)

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
    val source = sources(0)
    DependencyGraph.fromTurtle(inputGraph, source)

    // match patterns
    for (map <- DependencyGraph.executeSparql(inputGraph, denomQuery)) {
      // add to input graph for next query
      inputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toUri)
      // add to output graph for output
      outputGraph.addEdge(map("predicate"), map("subject"), map("object"), map("predicate").toUri)
    }

    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      addArgs(map("node"))
    }

    val sink: Writer = destinations(0)
    DependencyGraph.toTurtle(inputGraph, sink)
    DependencyGraph.toTurtle(outputGraph, sink)

    val debugSink: Writer = destinations(1)
    DependencyGraph.toTurtle(outputGraph, debugSink)

    inputGraph.shutdown()
    outputGraph.shutdown()
  }

  /** map input dependencies to output args */
  def addArgs(node: Vertex): Unit = {
    // define input dependency to output role relations
    val roles: Seq[(String, String)] = Seq(
      ("nsubj", "pred:agent"),
      ("nsubjpass", "pred:object"),
      ("dobj", "pred:object"),
      ("iobj", "pred:arg"),
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
    val filter: String = dep match {
      case "nsubj" => s"""FILTER NOT EXISTS { ?rcmod dep:rcmod <$uri> . 
                                              ?nsubj token:pos "WDT" . }"""
      case _ => "" // no filter
    }
    val query: String = s"""
      # find dep relation
      SELECT ?$dep WHERE {
        <$uri> dep:$dep ?$dep .
        FILTER NOT EXISTS { <$uri> dep:cop ?cop . }
        $filter
      }"""
    val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(inputGraph, query)
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
    val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(inputGraph, query)
    for (map <- result) {
      // use preposition as new predicate
      val prep: String = map("prep").toStringLiteral
      val obj: Vertex = map("obj")
      outputGraph.addEdge(null, node, obj, s"pred:$prep")
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
    val result: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(inputGraph, query)
    for (map <- result) {
      val rcmod: Vertex = map("rcmod")
      val rcmodUri: String = rcmod.toUri
      addArgs(rcmod)
      breakable {
        // find first missing pred
        for (role <- Seq("pred:agent", "pred:object", "pred:arg")) {
          val predQuery: String = s"""
          SELECT ?node WHERE {
            <$rcmodUri> $role ?node .
          }"""
          val predResult: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(outputGraph, predQuery)
          if (predResult.isEmpty) {
            outputGraph.addEdge(rcmod, rcmod, node, role)
            break
          }
        }
      }
    }
  }

}
