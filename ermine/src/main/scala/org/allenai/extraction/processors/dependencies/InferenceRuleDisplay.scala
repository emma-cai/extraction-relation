package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to generate pretty print version of inference rules */
object InferenceRuleDisplay extends TextProcessor {
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

  def separator(string: StringBuilder, prefix: String = ""): String = {
    if (string.isEmpty) {
      prefix
    } else {
      separator
    }
  }

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    DependencyGraph.fromTurtle(inputGraph, source)

    val sink: Writer = destinations(0)
    var ruleId: Int = 0
    // match patterns
    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      // rule id
      sink.write(s"display(rule${ruleId}, ")
      ruleId += 1

      val relation: String = nodeRelation(x, map("r"), y)
      sink.write(relation)

      sink.write('\n')
    }

    inputGraph.shutdown()
  }

  //TODO: collect list of vars, add var string definitions

  def nodeRelation(x: Vertex, r: Vertex, y: Vertex): String = {
    val xlabel: String = nodeArgs(x)
    val rel: String = r.toStringLiteral.toUpperCase
    val ylabel: String = nodeArgs(y)
    s"$rel($xlabel, $ylabel)"
  }

  def nodeArgs(node: Vertex, prefix: String = ""): String = {
    val uri: String = node.toUri
    val args = new StringBuilder()
    // add args
    for (role <- Seq("agent", "object", "arg")) {
      val argQuery: String = s"""
        SELECT ?arg WHERE {
          <$uri> pred:$role ?arg .
        }"""
      for (map <- DependencyGraph.executeSparql(inputGraph, argQuery)) {
        val arg: Vertex = map("arg")
        args ++= separator(args, prefix)
        args ++= nodeLabel(arg)
      }
    }
    // add PPs
    val prepQuery: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
        FILTER(?pred != "agent") .
        FILTER(?pred != "object") .
        FILTER(?pred != "arg") .
      }"""
    var firstPrep: String = ""
    for (map <- DependencyGraph.executeSparql(inputGraph, prepQuery)) {
      val pred = map("pred").toStringLiteral
      val arg: Vertex = map("arg")
      val argLabel = nodeLabel(arg)
      val argString = nodeString(arg)
      args ++= separator(args, prefix)
      if (firstPrep == "") {
        // attach first prep to verb
        firstPrep = '_' + pred
        args ++= argLabel
      } else {
        args ++= s"$pred($argLabel)"
      }
    }

    if (args.nonEmpty) {
      nodeLabel(node, false) + firstPrep + '(' + args.toString + ')'
    } else {
      nodeLabel(node)
    }
  }

  def nodeLabel(node: Vertex, initcap: Boolean = true): String = {
    val uri: String = node.toUri
    val id: String = uri.split("http://aristo.allenai.org/id#").last
    val query: String = s"""
      SELECT ?label WHERE {
        <$uri> rdfs:label ?label .
      }"""
    val result: Map[String, Vertex] = DependencyGraph.executeSparql(inputGraph, query).head
    val label = result.get("label").map(_.toStringLiteral).getOrElse("")
    if (initcap) {
      Character.toUpperCase(label.charAt(0)) + label.substring(1)
    } else {
      label
    }
  }

  def nodeString(node: Vertex, prefix: String = ""): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?string WHERE {
        <$uri> rdfs:comment ?string .
      }"""
    val result: Map[String, Vertex] = DependencyGraph.executeSparql(inputGraph, query).head
    val label: String = nodeLabel(node)
    val string = result.get("string").map(_.toStringLiteral).getOrElse("")
    s"""${prefix}$label = "$string""""
  }

}
