package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.FlatProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import java.io.Writer

/** processor to generate pretty print version of inference rules */
object InferenceRuleDisplay extends FlatProcessor {
  override def processText(source: Source, destination: Writer): Unit = {
    new InternalProcessor(source, destination).process()
  }

  /** Helper class, holding a reference to the IO graph. */
  class InternalProcessor(source: Source, destination: Writer) {
    val graph = new MemoryStoreSailGraph()
    DependencyGraph.fromTurtle(graph, source)
    DependencyGraph.setNamespaces(graph)

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

    def process(): Unit = {
      var ruleId: Int = 0
      // match patterns
      for (map <- DependencyGraph.executeSparql(graph, relQuery)) {
        val x: Vertex = map("x")
        val y: Vertex = map("y")
        // rule id
        destination.write(s"display(rule${ruleId}, ")
        ruleId += 1

        val relation: String = nodeRelation(x, map("r"), y)
        destination.write(relation)

        destination.write('\n')
      }

      graph.shutdown()
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
        for (map <- DependencyGraph.executeSparql(graph, argQuery)) {
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
      for (map <- DependencyGraph.executeSparql(graph, prepQuery)) {
        val pred = map("pred").toStringLiteral
        val arg: Vertex = map("arg")
        val argLabel = nodeLabel(arg)
        val argString = nodeString(arg)
        args ++= separator(args, prefix)
        args ++= s"$pred($argLabel)"
      }

      if (args.nonEmpty) {
        nodeLabel(node, false) + '(' + args.toString + ')'
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
      val result: Map[String, Vertex] = DependencyGraph.executeSparql(graph, query).head
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
      val result: Map[String, Vertex] = DependencyGraph.executeSparql(graph, query).head
      val label: String = nodeLabel(node)
      val string = result.get("string").map(_.toStringLiteral).getOrElse("")
      s"""${prefix}$label = "$string""""
    }

  }
}
