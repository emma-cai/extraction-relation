package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to add labels and string descriptions to extracted nodes */
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

    // find longest sequence of focus tokens
    val focusTokens: Array[Vertex] = longestSequence(tokens.toArray)
    println(focusTokens.toList)

    // find parent node of focus
    val focusNode: Option[Vertex] = parentNode(focusTokens)
    println(focusNode)

    val rule = new StringBuilder()

    // collect top-level: either related nodes or root
    var mentions: ArrayBuffer[Vertex] = ArrayBuffer()

    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      // LHS
      if (!mentions.contains(x)) {
        mentions += x
        if (!rule.isEmpty) {
          rule ++= separator
        }
        rule ++= nodeIsa(x, focusNode)
        if (!rule.isEmpty) {
          rule ++= separator
        }
        rule ++= nodeArgs(x)
      }
      if (!mentions.contains(y)) {
        mentions += y
        if (!rule.isEmpty) {
          rule ++= separator
        }
        rule ++= nodeIsa(y, focusNode)
        if (!rule.isEmpty) {
          rule ++= separator
        }
        rule ++= nodeArgs(y)
      }
      // relation
      if (!rule.isEmpty) {
        rule ++= separator
      }
      rule ++= questionRel(x, map("r"), y, focusNode)
    }
      // RHS
    rule ++= " -> Q="
    rule ++= nodeIsa(focusNode.get, None)
    rule ++= ".\n"

    val sink: Writer = destinations(0)
    // rule id
    var ruleId: Int = 0
    ruleId += 1
    sink.write(s"rule${ruleId}:: ")
    sink.write(rule.toString)

    // collect verbs with args

    // collect any non-arg tokens

    // use focus as RHS
    // replace focus with Q on LHS

    // add any relation on LHS

    /*
    var ruleId: Int = 0
    // match patterns
    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      val relation: String = nodeRel(x, map("r"), y)
      // setup, relation -> focus
      ruleId += 1
      sink.write(relationRule(x, relation, y, ruleId))
    }
     */

    inputGraph.shutdown()
  }

  def nodeParent(node: Vertex): Option[Vertex] = {
    val uri = node.toUri
    val nodeQuery =
      s"""SELECT ?parent WHERE {
        ?x ?rel <$uri> .
        FILTER(STRSTARTS(str(?rel), "http://nlp.stanford.edu/basic/")) .
      }"""
    val results: Seq[Map[String, Vertex]] = DependencyGraph.executeSparql(inputGraph, nodeQuery)
    results match {
      case Nil => None
      case head :: _ => Some(head("parent"))
    }
  }

  /*
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
 */

  def questionRel(x: Vertex, r: Vertex, y: Vertex, focus: Option[Vertex]): String = {
    val xlabel: String = nodeLabel(x, focus)
    val rel: String = r.toStringLiteral
    val ylabel: String = nodeLabel(y, focus)
    s"$rel($xlabel, $ylabel)"
  }

  def nodeArgs(node: Vertex, prefix: String = ""): String = {
    val uri: String = node.toUri
    val label: String = nodeLabel(node, None)
    val query: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
      }"""
    val args = new StringBuilder()
    for (map <- DependencyGraph.executeSparql(inputGraph, query)) {
      val pred = map("pred").toStringLiteral
      val argString = nodeString(map("arg"))
      if (args.isEmpty)
        args ++= prefix
      else
        args ++= separator
      args.append(s"""$pred($label, "$argString")""")
    }
    args.toString
  }

  def nodeIsa(node: Vertex, focus: Option[Vertex], prefix: String = ""): String = {
    if (Some(node) == focus) {
      return ""
    }
    val label: String = nodeLabel(node, None)
    val string: String = nodeString(node)
    s"""${prefix}isa($label, "$string")"""
  }

  def nodeLabel(node: Vertex, focus: Option[Vertex]): String = {
    if (Some(node) == focus) {
      return "Q"
    }
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
  
  /** find longest sequence of tokens */
  private def longestSequence(tokens: Array[Vertex]): Array[Vertex] = {
    // sort by sentence and token position
    val sortedTokens: Array[Vertex] = tokens.sortBy(t => (t.sentenceId, t.tokenId)).toArray
    // TODO: cleaner way with filter?
    var maxStart: Int = 0
    var maxLength: Int = 1
    var currentStart: Int = 0
    var currentLength: Int = 1
    for (i <- 0 to sortedTokens.length - 2) {
      if (sortedTokens(i).sentenceId == sortedTokens(i+1).sentenceId
        && sortedTokens(i).tokenId + 1 == sortedTokens(i+1).tokenId) {
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
  
  private def parentNode(tokens: Array[Vertex]): Option[Vertex] = {
    // walk up the tree to find the lowest node covering all focus tokens
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
  
}
