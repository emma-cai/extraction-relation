package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer


/** processor to add labels and string descriptions to extracted nodes */
object InferenceRules extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  // SPARQL query for nodes with added rel: relation
  val relQuery =
    """SELECT ?x ?r ?y WHERE {
      ?x ?rel ?y .
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
      BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/rel/") AS ?r) .
    }"""

  val separator = ", "

  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    val sink: Writer = destinations(0)

    val graph = new DependencyGraph()
    graph.loadTurtle(source)

    var ruleId: Int = 0
    // match patterns
    for (map <- graph.executeQuery(relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      val relation: String = nodeRel(x, map("r"), y, graph)
      // left -> relation, right
      ruleId += 1
      sink.write(relationRule(x, relation, y, ruleId, graph))
      // right -> relation, left
      ruleId += 1
      sink.write(relationRule(y, relation, x, ruleId, graph))
    }

    graph.shutdown()
  }

  def relationRule(left: Vertex, relation: String, right: Vertex, ruleId: Int, graph: DependencyGraph): String = {
    val rule = new StringBuilder()
    // rule id
    rule ++= s"rule${ruleId}:: "
    // LHS
    rule ++= nodeIsa(left, graph)
    rule ++= nodeArgs(left, graph, separator)
    // relation
    rule ++= " -> " + relation
    // RHS
    rule ++= nodeIsa(right, graph, separator)
    rule ++= nodeArgs(right, graph, separator)

    rule ++= ".\n"
    rule.toString
  }

  def nodeRel(x: Vertex, r: Vertex, y: Vertex, graph: DependencyGraph): String = {
    val xlabel: String = nodeLabel(x, graph)
    val rel: String = r.toLiteral
    val ylabel: String = nodeLabel(y, graph)
    s"$rel($xlabel, $ylabel)"
  }

  def nodeArgs(node: Vertex, graph: DependencyGraph, prefix: String = ""): String = {
    val uri: String = node.toUri
    val label: String = nodeLabel(node, graph)
    val query: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
      }"""
    val args = new StringBuilder()
    for (map <- graph.executeQuery(query)) {
      val pred = map("pred").toLiteral
      val argString = nodeString(map("arg"), graph)
      if (args.isEmpty)
        args ++= prefix
      else
        args ++= separator
      args.append(s"$pred($label, $argString)")
    }
    args.toString
  }

  def nodeIsa(node: Vertex, graph: DependencyGraph, prefix: String = ""): String = {
    val label: String = nodeLabel(node, graph)
    val string: String = nodeString(node, graph)
    s"${prefix}isa($label, $string)"
  }

  def nodeLabel(node: Vertex, graph: DependencyGraph): String = {
    val uri: String = node.toUri
    val id: String = uri.split("http://aristo.allenai.org/id#").last
    val query: String = s"""
      SELECT ?label WHERE {
        <$uri> rdfs:label ?label .
      }"""
    val result: Map[String,Vertex] = graph.executeQuery(query).head
    val label = result.get("label").map(_.toLiteral).getOrElse("")
    s"E$id-$label"
  }

  def nodeString(node: Vertex, graph: DependencyGraph): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?string WHERE {
        <$uri> rdfs:comment ?string .
      }"""
    val result: Map[String,Vertex] = graph.executeQuery(query).head
    result.get("string").map(_.toUri).getOrElse("")
  }

}
