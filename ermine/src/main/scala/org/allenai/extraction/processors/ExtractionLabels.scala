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
  override val numInputs = 4
  override val numOutputs = 1

  // SPARQL query for nodes with added pred: relations
  val predQuery: String =
    """SELECT ?x ?rel ?y WHERE {
      ?x ?rel ?y .
      FILTER(STRSTARTS(str(?rel), "http://aristo.allenai.org/pred/")) .
    }"""

  // SPARQL query for nodes with added rel: relations
  val relQuery: String =
    """SELECT ?x WHERE {
      { ?x ?rel ?y . }
      UNION
      { ?y ?rel ?x . }
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

  override protected def processText(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val graph = new DependencyGraph()
    for (source <- sources) {
      graph.loadTurtle(source)
    }

    // match verbal predicates
    for (map <- graph.executeQuery(predQuery)) {
      val xnode = map("x")
      addLabel(xnode, graph)
      addText(xnode, VerbExcludeString, graph)
      val ynode = map("y")
      addLabel(ynode, graph)
      addText(ynode, ArgExcludeString, graph)
    }
    // match unlabeled relations
    for (map <- graph.executeQuery(relQuery)) {
      addLabel(map("x"), graph)
      addText(map("x"), ArgExcludeString, graph)
    }

    val sink: Writer = destinations(0)
    graph.saveTurtle(sink)
    graph.shutdown()
  }

  /** use token lemma as label */
  def addLabel(node: Vertex, graph: DependencyGraph): Edge = {
    val label: String = graph.tokenInfo(node, "lemma")
    val v: Vertex = graph.outputGraph.addVertex('"' + label + '"')
    graph.outputGraph.addEdge(label, node, v, "rdfs:label")
  }

  /** create complete string for node */
  def addText(node: Vertex, exclude: String = "", graph: DependencyGraph): Edge = {
    val constits: Seq[Vertex] = (graph.nodeConstits(node, exclude) :+ node).sortWith(_ < _)
    val tokens: Seq[String] = constits.map(x => graph.tokenInfo(x))
    val text: String = tokens.mkString(" ")
    val v: Vertex = graph.outputGraph.addVertex('"' + text + '"')
    graph.outputGraph.addEdge(text, node, v, "rdfs:comment")
  }

}
