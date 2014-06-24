package org.allenai.extraction.processors.dependencies

import org.allenai.extraction.rdf.{ DependencyGraph, TurtleProcessor }
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.{ Edge, Vertex }
import com.tinkerpop.blueprints.impls.sail.SailGraph

/** processor to map dependencies of extracted nodes to roles */
object ExtractionLabels extends TurtleProcessor {
  override def processGraph(graph: SailGraph): Unit = {
    new InternalProcessor(graph).process()
  }

  /** Helper class, holding a reference to the IO graph. */
  class InternalProcessor(graph: SailGraph) {
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

    val VerbExcludeString: Option[String] = Some(
      // list of dependencies to exclude when building verb string
      Set("aux", "auxpass", "nsubj", "nsubjpass", "csubj", "csubjpass", "dobj", "iobj", "xcomp",
        "prep", "conj", "cc", "mark", "advcl", "advmod", "npadvmod", "tmod", "acomp", "dep",
        "ccomp", "cop", "expl", "attr", "xsubj", "purpcl", "vmod", "rcmod",
        "partmod").mkString("|"))

    val ArgExcludeString: Option[String] = Some(
      // list of dependencies to exclude when building arg string
      Set("conj", "cc", "appos", "dep", "xcomp", "infmod", "rcmod", "partmod", "advmod", "cop",
        "nsubj", "aux", "ref", "vmod").mkString("|"))

    /** use token lemma as label */
    def addLabel(node: Vertex): Edge = {
      val label: String = DependencyGraph.tokenInfo(graph, node, "lemma")
      val v: Vertex = graph.addVertex('"' + label + '"')
      graph.addEdge(label, node, v, "rdfs:label")
    }

    /** create complete string for node */
    def addText(node: Vertex, exclude: Option[String] = None): Edge = {
      val constits: Seq[Vertex] = (DependencyGraph.nodeConstits(graph, node, exclude) :+ node).sortBy(_.tokenId)
      val tokens: Seq[String] = constits.map(x => DependencyGraph.tokenInfo(graph, x, "text"))
      val text: String = tokens.mkString(" ")
      val v: Vertex = graph.addVertex('"' + text + '"')
      graph.addEdge(text, node, v, "rdfs:comment")
    }

    def process(): Unit = {
      // match verbal predicates
      for (map <- DependencyGraph.executeSparql(graph, predQuery)) {
        val xnode = map("x")
        addLabel(xnode)
        addText(xnode, VerbExcludeString)
        val ynode = map("y")
        addLabel(ynode)
        addText(ynode, ArgExcludeString)
      }
      // match unlabeled relations
      for (map <- DependencyGraph.executeSparql(graph, relQuery)) {
        addLabel(map("x"))
        addText(map("x"), ArgExcludeString)
      }
    }
  }
}
