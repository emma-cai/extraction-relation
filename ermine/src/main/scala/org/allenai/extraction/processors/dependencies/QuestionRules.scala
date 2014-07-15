package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.ErmineException
import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to generate Arilog inference rule format of questions
  * - requires a focus string as a second input
  */
object QuestionRules extends TextProcessor {
  override val numInputs = 2
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

  def separator(string: StringBuilder): String = {
    if (string.nonEmpty) {
      separator
    } else {
      ""
    }
  }

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    DependencyGraph.fromTurtle(inputGraph, source)

    val focusSource = sources(1)
    val focus = focusSource.getLines.mkString("\n")
    focusSource.close()

    // find all focus tokens in graph
    var tokens: ArrayBuffer[Vertex] = ArrayBuffer()
    for (word <- focus.split(' ')) {
      val tokenQuery = s"""SELECT ?token WHERE {
          ?token token:text "$word" .
        }"""
      for (map <- DependencyGraph.executeSparql(inputGraph, tokenQuery)) {
        tokens += map("token")
      }
    }
    if (tokens.isEmpty) {
      throw new ErmineException(s"Focus string '${focus}' not found in input text.")
    }

    // find longest sequence of focus tokens
    val focusTokens: Array[Vertex] = longestSequence(tokens.toArray)
    // find parent node of focus
    val focusNode: Option[Vertex] = parentNode(focusTokens)
    if (focusTokens.isEmpty) {
      throw new ErmineException(s"Parent node for focus string '${focus}' not found in parse.")
    }

    // rule id
    var ruleId: Int = 0
    val sink: Writer = destinations(0)
    val rule = new StringBuilder()
    val relation = new StringBuilder()

    // collect top-level: either related nodes or root
    var mentions: ArrayBuffer[Vertex] = ArrayBuffer(focusNode.get)
    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      // LHS
      if (!mentions.contains(x)) {
        rule ++= separator(rule)
        rule ++= nodeRule(x, mentions.toSet, separator)
        mentions += x
      }
      if (!mentions.contains(y)) {
        rule ++= separator(rule)
        rule ++= nodeRule(y, mentions.toSet, separator)
        mentions += y
      }
      if (x == focusNode.get || y == focusNode.get) {
        // RHS
        relation ++= separator(relation)
        relation ++= nodeRel(x, map("r"), y)
      } else {
        // LHS
        rule ++= separator(rule)
        rule ++= nodeRel(x, map("r"), y)
      }
    }
    // relation
    rule ++= " -> "
    rule ++= relation
    // focus
    rule ++= separator
    rule ++= nodeRule(focusNode.get, Set(), separator)

    ruleId += 1
    sink.write(s"rule${ruleId}:: ")
    sink.write(rule.toString)
    sink.write(".\n")

    inputGraph.shutdown()
  }

  def nodeRel(x: Vertex, r: Vertex, y: Vertex): String = {
    val xlabel: String = nodeLabel(x)
    val rel: String = r.toStringLiteral
    val ylabel: String = nodeLabel(y)
    s"$rel($xlabel, $ylabel)"
  }

  def nodeRule(node: Vertex, skip: Set[Vertex], prefix: String = ""): String = {
    val string = new StringBuilder()
    // node plus any args
    string ++= nodeIsa(node)
    string ++= nodeArgs(node, skip, prefix)
    string ++= rcmodArgs(node, skip, prefix)
    string.toString
  }

  def nodeIsa(node: Vertex, prefix: String = ""): String = {
    val label: String = nodeLabel(node)
    val string: String = nodeString(node)
    s"""${prefix}isa($label, "$string")"""
  }

  def nodeArgs(node: Vertex, skip: Set[Vertex], prefix: String = ""): String = {
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
      if (!skip.contains(arg)) {
        args ++= separator
        args.append(s"""isa($argLabel, "$argString")""")
      }
    }
    args.toString
  }

  def rcmodArgs(node: Vertex, skip: Set[Vertex], prefix: String = ""): String = {
    val uri: String = node.toUri
    val label: String = nodeLabel(node)
    val query: String = s"""
      SELECT ?rcmod WHERE {
        <$uri> dep:rcmod ?rcmod .
      }"""
    val args = new StringBuilder()
    for (map <- DependencyGraph.executeSparql(inputGraph, query)) {
      val rcmod: Vertex = map("rcmod")
      if (args.isEmpty) {
        args ++= prefix
      } else {
        args ++= separator
      }
      args ++= nodeArgs(rcmod, skip)
    }
    args.toString
  }

  def nodeLabel(node: Vertex): String = {
    val uri: String = node.toUri
    val id: String = uri.split("http://aristo.allenai.org/id#").last
    val query: String = s"""
      SELECT ?label WHERE {
        <$uri> rdfs:label ?label .
      }"""
    val result: Map[String, Vertex] = DependencyGraph.executeSparql(inputGraph, query).head
    val label = result.get("label").map(_.toStringLiteral).getOrElse("")
    s"E$id-$label"
  }

  def nodeString(node: Vertex): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?string WHERE {
        <$uri> rdfs:comment ?string .
      }"""
    val result: Map[String, Vertex] = DependencyGraph.executeSparql(inputGraph, query).head
    result.get("string").map(_.toStringLiteral).getOrElse("")
  }

  /** find longest sequence of contiguous tokens */
  private[dependencies] def longestSequence(tokens: Array[Vertex]): Array[Vertex] = {
    // sort by sentence and token position
    val sortedTokens: Array[Vertex] = tokens.sortBy(t => (t.sentenceId, t.tokenId)).toArray
    // TODO: cleaner way with filter?
    var maxStart: Int = 0
    var maxLength: Int = 1
    var currentStart: Int = 0
    var currentLength: Int = 1
    for (i <- 0 to sortedTokens.length - 2) {
      if (sortedTokens(i).sentenceId == sortedTokens(i + 1).sentenceId
        && sortedTokens(i).tokenId + 1 == sortedTokens(i + 1).tokenId) {
        currentLength += 1
      } else {
        currentStart = i + 1
        currentLength = 1
      }
      if (currentLength > maxLength) {
        maxLength = currentLength
        maxStart = currentStart
      }
    }
    sortedTokens.slice(maxStart, maxStart + maxLength)
  }

  /** walk up the tree to find the lowest node covering all focus tokens */
  def parentNode(tokens: Array[Vertex]): Option[Vertex] = {
    var top: Option[Vertex] = None
    var parent: Option[Vertex] = Some(tokens.last)
    while (parent.isDefined) {
      val parentConstits: Seq[Vertex] = DependencyGraph.nodeConstits(inputGraph, parent.get) :+ parent.get
      if (tokens.toSet.subsetOf(parentConstits.toSet)) {
        top = parent
        parent = None
      } else {
        parent = nodeParent(parent.get)
      }
    }
    top
  }

  /** query basic dependency tree for immediate parent */
  def nodeParent(node: Vertex): Option[Vertex] = {
    val uri = node.toUri
    val nodeQuery =
      s"""SELECT ?parent WHERE {
        ?x ?rel <$uri> .
        FILTER(STRSTARTS(str(?rel), "http://nlp.stanford.edu/basic/")) .
      }"""
    val results: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(inputGraph, nodeQuery)
    results.headOption map { head => head("parent") }
  }

}
