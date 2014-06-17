package org.allenai.extraction.processors

import org.allenai.extraction.TextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import scala.io.Source

import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.Vertex
import java.io.Writer

/** processor to map dependencies of extracted nodes to roles */
object ExtractionLabels extends TextProcessor {
  override val numInputs = 1
  override val numOutputs = 2

  val inputGraph = new DependencyGraph()
  val outputGraph = new DependencyGraph()

  // SPARQL query for nodes with added pred: relations
  val predQuery: String =
    """SELECT ?x ?y WHERE {
      ?x ?rel ?y .
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
    }"""

  // SPARQL query for nodes with added rel: relations
  val relQuery: String =
    """SELECT ?x WHERE {
      { ?x ?rel ?y . } UNION { ?y ?rel ?x . }
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/rel/")) .
      FILTER NOT EXISTS { ?x rdfs:label ?l . } 
    }"""

  val VerbExcludeString: String = {
    // list of dependencies to exclude when building verb string
    Set("aux", "auxpass", "nsubj", "nsubjpass", "csubj", "csubjpass", "dobj", "iobj", "xcomp", "prep", "conj", "cc", "mark", "advcl", "advmod", "npadvmod", "tmod", "acomp", "dep", "ccomp", "cop", "expl", "attr", "xsubj", "purpcl", "vmod", "rcmod", "partmod").mkString("|")
  }

  val ArgExcludeString: String = {
    // list of dependencies to exclude when building arg string
    Set("conj", "cc", "appos", "dep", "xcomp", "infmod", "rcmod", "partmod", "advmod", "cop", "nsubj", "aux", "ref", "vmod").mkString("|")
  }

  override def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val source = sources(0)
    inputGraph.loadTurtle(source)

    // match verbal predicates
    for (map <- inputGraph.executeQuery(predQuery)) {
      val xnode = map("x")
      addLabel(xnode)
      addText(xnode, VerbExcludeString)
      val ynode = map("y")
      addLabel(ynode)
      addText(ynode, ArgExcludeString)
    }
    // match unlabeled relations
    for (map <- inputGraph.executeQuery(relQuery)) {
      addLabel(map("x"))
      addText(map("x"), ArgExcludeString)
    }

    val sink: Writer = destinations(0)
    inputGraph.saveTurtle(sink)
    outputGraph.saveTurtle(sink)

    val debugSink: Writer = destinations(1)
    outputGraph.saveTurtle(debugSink)

    inputGraph.shutdown()
    outputGraph.shutdown()
  }

  /** use token lemma as label */
  def addLabel(node: Vertex): Edge = {
    val label: String = inputGraph.tokenInfo(node, "lemma")
    val v: Vertex = outputGraph.addVertex('"' + label + '"')
    outputGraph.addEdge(label, node, v, "rdfs:label")
  }

  /** create complete string for node */
  def addText(node: Vertex, exclude: String = ""): Edge = {
    val constits: Seq[Vertex] = (inputGraph.nodeConstits(node, exclude) :+ node).sortWith(_ < _)
    val tokens: Seq[String] = constits.map(x => inputGraph.tokenInfo(x))
    val text: String = tokens.mkString(" ")
    val v: Vertex = outputGraph.addVertex('"' + text + '"')
    outputGraph.addEdge(text, node, v, "rdfs:comment")
  }

}
