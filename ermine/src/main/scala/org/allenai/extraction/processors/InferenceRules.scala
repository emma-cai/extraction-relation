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

  val inputGraph = new DependencyGraph()

  // SPARQL query for nodes with added rel: relation
  val relQuery =
    """SELECT ?x ?r ?y WHERE {
      ?x ?rel ?y .
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
      BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/rel/") AS ?r) .
    }"""

  val separator = ", "

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    inputGraph.loadTurtle(source)

    val sink: Writer = destinations(0)
    var ruleId: Int = 0
    // match patterns
    for (map <- inputGraph.executeQuery(relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      val relation: String = nodeRel(x, map("r"), y)
      // left -> relation, right
      ruleId += 1
      sink.write(relationRule(x, relation, y, ruleId))
      // right -> relation, left
      ruleId += 1
      sink.write(relationRule(y, relation, x, ruleId))
    }

    inputGraph.shutdown()
  }

  def relationRule(left: Vertex, relation: String, right: Vertex, ruleId: Int): String = {
    val rule = new StringBuilder()
    // rule id
    rule ++= s"rule${ruleId}:: "
    // LHS
    rule ++= nodeIsa(left)
    rule ++= nodeArgs(left, separator)
    // relation
    rule ++= " -> " + relation
    // RHS
    rule ++= nodeIsa(right, separator)
    rule ++= nodeArgs(right, separator)

    rule ++= ".\n"
    rule.toString
  }

  def nodeRel(x: Vertex, r: Vertex, y: Vertex): String = {
    val xlabel: String = nodeLabel(x)
    val rel: String = r.toLiteral
    val ylabel: String = nodeLabel(y)
    s"$rel($xlabel, $ylabel)"
  }

  def nodeArgs(node: Vertex, prefix: String = ""): String = {
    val uri: String = node.toUri
    val label: String = nodeLabel(node)
    val query: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
      }"""
    val args = new StringBuilder()
    for (map <- inputGraph.executeQuery(query)) {
      val pred = map("pred").toLiteral
      val argString = nodeString(map("arg"))
      if (args.isEmpty)
        args ++= prefix
      else
        args ++= separator
      args.append(s"$pred($label, $argString)")
    }
    args.toString
  }

  def nodeIsa(node: Vertex, prefix: String = ""): String = {
    val label: String = nodeLabel(node)
    val string: String = nodeString(node)
    s"${prefix}isa($label, $string)"
  }

  def nodeLabel(node: Vertex): String = {
    val uri: String = node.toUri
    val id: String = uri.split("http://aristo.allenai.org/id#").last
    val query: String = s"""
      SELECT ?label WHERE {
        <$uri> rdfs:label ?label .
      }"""
    val result: Map[String, Vertex] = inputGraph.executeQuery(query).head
    val label = result.get("label").map(_.toLiteral).getOrElse("")
    s"E$id-$label"
  }

  def nodeString(node: Vertex): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?string WHERE {
        <$uri> rdfs:comment ?string .
      }"""
    val result: Map[String, Vertex] = inputGraph.executeQuery(query).head
    result.get("string").map(_.toUri).getOrElse("")
  }

}
