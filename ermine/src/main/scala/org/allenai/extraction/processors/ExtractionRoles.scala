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
  override val numInputs = 1
  override val numOutputs = 1

  // SPARQL query for nodes with added rel: relation
  val query: String =
    """SELECT ?x ?y WHERE {
      ?x ?rel ?y .
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
    }"""

  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    val graph = new DependencyGraph()
    graph.loadTurtle(source)

    // match patterns
    for {
      map <- graph.executeQuery(query)
      node <- Seq(map("x"), map("y"))
    } {
      addArgs(node, graph)
    }

    val sink: Writer = destinations(0)
    graph.saveTurtle(sink)
    graph.shutdown()
  }

  /** map input dependencies to output args */
  def addArgs(node: Vertex, graph: DependencyGraph) = {
    // define input dependency to output role relations
    val roles: Seq[Tuple2[String,String]] = Seq(("nsubj","pred:agent"),
                                                ("dobj","pred:object"),
                                                ("iobj","pred:arg"),
                                                ("tmod","pred:arg"))
    for ((dep, role) <- roles) {
      addArg(node, dep, role, graph)
    }
    addPreps(node, graph)
  }

  /** query for arg values and add to outputGraph */
  def addArg(node: Vertex, dep: String, role: String, graph: DependencyGraph) = {
    val uri: String = node.toUri
    val query: String = s"""
      # find dep relation
      SELECT ?$dep WHERE {
        <$uri> dep:$dep ?$dep .
      }"""
    val result: Seq[Map[String,Vertex]] = graph.executeQuery(query)
    for (map <- result) {
      val obj: Vertex = map(dep)
      graph.outputGraph.addEdge(null, node, obj, role)
    }
  }

  /** query for PP values and add to outputGraph */
  def addPreps(node: Vertex, graph: DependencyGraph) = {
    val uri: String = node.toUri
    val query: String = s"""
      # find prep_* relation and extract the preposition substring
      SELECT ?prep ?obj WHERE {
        <$uri> ?dep ?obj .
        FILTER(CONTAINS(str(?dep), "/dep/prep_")) .
        BIND(STRAFTER(str(?dep), "_") AS ?prep) .
      }"""
    val result: Seq[Map[String,Vertex]] = graph.executeQuery(query)
    for (map <- result) {
      // use preposition as new predicate
      val prep: String = map("prep").toLiteral
      val obj: Vertex = map("obj")
      graph.outputGraph.addEdge(null, node, obj, s"pred:$prep")
    }
  }

}
