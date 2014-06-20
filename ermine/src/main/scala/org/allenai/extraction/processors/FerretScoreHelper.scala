package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.collection.mutable
import scala.io.Source

import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to add labels and string descriptions to extracted nodes */
object FerretScoreHelper extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 1

  val inputGraph = new MemoryStoreSailGraph()
  val sentenceTokenCache = mutable.Map[Int, Seq[Vertex]]()
  val sentenceStringCache = mutable.Map[Int, String]()
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
    // match patterns
    for (map <- DependencyGraph.executeSparql(inputGraph, relQuery)) {
      val x: Vertex = map("x")
      val y: Vertex = map("y")
      val xElements: Vector[(String, Vertex)] = nodeElements(x)
      val yElements: Vector[(String, Vertex)] = nodeElements(y)
      val nodes = (for ((id, node) <- xElements ++ yElements) yield node).toList.distinct
      val sentenceNumbers = (nodes map nodeSentenceNumber).distinct
      val sentences: List[String] = for (num <- sentenceNumbers) yield sentenceStringCache.getOrElseUpdate(num, getSentence(num))

      val rel = map("r")

      sink.write(sentences.mkString("\n"))
      sink.write("\n")

      sink.write("  :: ")
      sink.write(renderRule(xElements, rel, yElements))
      sink.write("\n\n")
    }

    inputGraph.shutdown()
  }

  def renderVertex(v: Vertex) = "\"" + nodeString(v) + "\""

  def renderRelation(rel: Vertex): String = "--" + rel.toStringLiteral.toUpperCase + "->"

  def renderRuleArgument(elements: Vector[(String, Vertex)]): String = {
    val emptyArgString = "-"
    val stdArgs = List("isa", "agent", "object")
    if (elements.length == 1 && elements.head._1 == "isa") renderVertex(elements.head._2) else {
      val map = elements.toMap
      val agent = map.get("agent") map renderVertex getOrElse (emptyArgString)
      val rel = map.get("isa") map renderVertex getOrElse (emptyArgString)
      val object0 = map.get("object") map renderVertex getOrElse (emptyArgString)
      val preps = for {
        (arg, vertex) <- elements
        if !stdArgs.contains(arg)
      } yield arg.toUpperCase + ":" + renderVertex(vertex)
      val prepString = if (preps.isEmpty) "" else preps.mkString(" ", " ", "")
      s"[ $agent $rel $object0$prepString ]"
    }

  }

  def renderRule(xElements: Vector[(String, Vertex)], rel: Vertex, yElements: Vector[(String, Vertex)]): String = {
    renderRuleArgument(xElements) + " " + renderRelation(rel) + " " + renderRuleArgument(yElements)
  }

  def getSentence(sentenceNum: Int) = {
    val query: String = s"""
      SELECT ?uri ?text ?begin WHERE {
        ?uri <http://nlp.stanford.edu/token/text> ?text .
        FILTER(STRSTARTS(str(?uri), "http://aristo.allenai.org/id#${sentenceNum}_"))
        ?uri <http://nlp.stanford.edu/token/begin> ?begin .
      }"""
    val res = for {
      map <- DependencyGraph.executeSparql(inputGraph, query)
    } yield (map("begin").toIntLiteral, map("text").toStringLiteral)
    res.sorted.map(_._2).mkString(" ")
  }

  def nodeSentenceNumber(node: Vertex): Int =
    """http://aristo.allenai.org/id#(\d+)""".r.findFirstMatchIn(node.toUri).map(_.group(1)).getOrElse("0").toInt

  /* Gives Vector("isa" -> vertex1, "agent" -> vertex2, ...) */
  def nodeElements(node: Vertex): Vector[(String, Vertex)] = {
    val uri: String = node.toUri
    val label: String = nodeLabel(node)
    val query: String = s"""
      SELECT ?pred ?arg WHERE {
        <$uri> ?rel ?arg .
        FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
        BIND(STRAFTER(str(?rel), "http://aristo.allenai.org/pred/") AS ?pred) .
      }"""

    val args = new StringBuilder()
    val res = for {
      map <- DependencyGraph.executeSparql(inputGraph, query)
      pred = map("pred").toStringLiteral
    } yield pred -> map("arg")
    res.toVector :+ ("isa" -> node)
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
      val argString = nodeString(map("arg"))
      if (args.isEmpty)
        args ++= prefix
      else
        args ++= separator
      args.append(s"""$pred($label, "$argString")""")
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

}
