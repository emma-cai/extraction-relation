package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.{ ErmineException, TextProcessor }
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to generate Arilog inference rule format */
object InferenceRules extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  val inputGraph = new MemoryStoreSailGraph()
  DependencyGraph.setNamespaces(inputGraph)

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
    DependencyGraph.fromTurtle(inputGraph, source)

    val sink: Writer = destinations(0)
    var ruleId: Int = 0
    // match patterns
    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
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
    val rel: String = r.toStringLiteral
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
    for (map <- DependencyGraph.executeSparql(inputGraph, query)) {
      val pred = map("pred").toStringLiteral
      val arg: Vertex = map("arg")
      val argLabel = nodeLabel(arg)
      val argString = nodeString(arg)
      if (args.isEmpty) {
        args ++= prefix
      } else {
        args ++= separator
      }
      args.append(s"$pred($label, $argLabel)")
      args ++= separator
      args.append(s"""isa($argLabel, "$argString")""")
    }
    args.toString
  }

  def nodeIsa(node: Vertex, prefix: String = ""): String = {
    val label: String = nodeLabel(node)
    val string: String = nodeString(node)
    s"""${prefix}isa($label, "$string")"""
  }

  def nodeLabel(node: Vertex): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?label WHERE {
        <$uri> rdfs:label ?label .
      }"""
    val labelOption: Option[String] = for {
      result <- DependencyGraph.executeSparql(inputGraph, query).headOption
      vertex <- result.get("label")
    } yield vertex.toStringLiteral
    labelOption match {
      case Some(label) => s"E${node.sentenceId}S${node.tokenId}-$label"
      case None => throw new ErmineException(s"Couldn't find rdfs:label for $uri")
    }
  }

  def nodeString(node: Vertex): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?string WHERE {
        <$uri> rdfs:comment ?string .
      }"""
    val string: Option[String] = for {
      result <- DependencyGraph.executeSparql(inputGraph, query).headOption
      vertex <- result.get("string")
    } yield vertex.toStringLiteral
    string getOrElse { "" }
  }
}
