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
    val graph = new DependencyGraph()
    graph.loadTurtle(source)

    val sb = new StringBuilder()
    var ruleId: Int = 1
    // match patterns
    for (map <- graph.executeQuery(relQuery)) {
      val x = map("x")
      val y = map("y")

      // left -> relation, right
      // rule id
      sb.append(s"rule${ruleId}:: ")
      ruleId += 1
      // LHS
      sb.append(nodeIsa(x, graph))
      sb.append(nodeArgs(x, graph, separator))
      // relation
      sb.append(" -> " + nodeRel(x, map("r"), y, graph))
      // RHS
      sb.append(nodeIsa(y, graph, separator))
      sb.append(nodeArgs(y, graph, separator))
      sb ++= ".\n"

      // right -> relation, left
      // rule id
      sb.append(s"rule${ruleId}:: ")
      ruleId += 1
      // LHS
      sb.append(nodeIsa(y, graph))
      sb.append(nodeArgs(y, graph, separator))
      // relation
      sb.append(" -> " + nodeRel(x, map("r"), y, graph))
      // RHS
      sb.append(nodeIsa(x, graph, separator))
      sb.append(nodeArgs(x, graph, separator))
      sb ++= ".\n"
    }

    graph.shutdown()
    val sink: Writer = destinations(0)
    sink.write(sb.toString)
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
    val sb = new StringBuilder()
    for (map <- graph.executeQuery(query)) {
      val pred = map("pred").toLiteral
      val argString = nodeString(map("arg"), graph)
      if (sb.isEmpty)
        sb ++= prefix
      else
        sb ++= separator
      sb.append(s"$pred($label, $argString)")
    }
    sb.toString
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
