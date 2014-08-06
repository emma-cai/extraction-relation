package org.allenai.relation.util
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.parse.PolytreeParser
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph
import org.allenai.nlpstack.graph.Graph
import org.allenai.nlpstack.parse.graph.TokenDependencyNode
//org.allenai.nlpstack.core.graph.Graph[org.allenai.nlpstack.core.parse.graph.TokenDependencyNode]

/** Dependency parser running through nlpstack. Currently hardcoded to use the polytree parser. */
class Polyparser {
  val parseFunction = new PolytreeParser().dependencyGraph(defaultTokenizer, defaultPostagger)_
  def main(args: Array[String]) = {
    processText("Animals cannot make their own food, so they need to eat to get nutrients.")
  }

  def processText(text: String): org.allenai.nlpstack.graph.Graph[org.allenai.nlpstack.parse.graph.TokenDependencyNode] = {
    val outputGraph = new MemoryStoreSailGraph()
    DependencyGraph.setNamespaces(outputGraph)

    try {
      //basic graph
      //      // Run the text through the NLP stack linewise.
      //      val stemmer = new MorphaStemmer()
      //      // First pass crease an ID-based graph.
      //      val (tokens, rawGraph) = parseFunction(text)
      //      // Second pass builds a lemmatized graph with edges holding tokens.
      //      val parseGraph = rawGraph.tokenized(tokens map stemmer.lemmatizePostaggedToken)
      //      return parseGraph

      //collapsed graph
      val stemmer = new MorphaStemmer()
      val (tokens, rawGraph) = parseFunction(text)
      val rawCollapsedGraph = rawGraph.collapse
      val collapsedGraph = rawCollapsedGraph.tokenized(tokens map stemmer.lemmatizePostaggedToken)
      return collapsedGraph
    } catch {
      case e: Exception => None
    }

    return null
  }

  /** Given a string-name, find the its id in the tree;
    * one string-name may appear multiple times in the tree (with different ids)
    */
  def getid(ns: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode], name: String): List[Int] = {
    //    var idlist: List[Int] = List()
    //
    //    var namearr = name.split(" ")
    //    ns.foreach {
    //      case p => {
    //        if (namearr.contains(p.string) && !idlist.contains(p.id))
    //          idlist = idlist ::: List(p.id)
    //      }
    //    }
    //    return idlist

    var idlist: List[Int] = List()
    ns.foreach {
      case p => {
        if (p.string.equals(name) && !idlist.contains(p.id))
          idlist = idlist ::: List(p.id)
      }
    }
    return idlist
  }

  /** get all dest-nodes given a source-node
    */
  def getDestNodes(srcNodeID: Int,
    ds: List[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]): List[String] = {
    var destNodeNames: List[String] = List()
    for (edge <- ds) {
      if (edge.source.id == srcNodeID) {
        if (!destNodeNames.contains(edge.dest.string))
          destNodeNames = destNodeNames ::: List(edge.dest.string)
      }
    }
    return destNodeNames
  }
}
