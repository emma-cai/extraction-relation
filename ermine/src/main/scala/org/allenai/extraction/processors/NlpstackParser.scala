package org.allenai.extraction.processors

import org.allenai.extraction.FlatProcessor
import org.allenai.extraction.rdf.{ DependencyGraph, Token }
import org.allenai.extraction.rdf.DependencyGraph.GraphRdf
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.parse.PolytreeParser
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

import java.io.Writer

/** Dependency parser running through nlpstack. Currently hardcoded to use the polytree parser. */
object NlpstackParser extends FlatProcessor {
  val parseFunction = new PolytreeParser().dependencyGraph(defaultTokenizer, defaultPostagger)_

  override def processText(source: Source, destination: Writer): Unit = {
    val corpus = Token.corpus(source)
    // The output graph we're generating.
    val outputGraph = new MemoryStoreSailGraph()
    DependencyGraph.setNamespaces(outputGraph)

    // Run the text through the NLP stack linewise.
    val stemmer = new MorphaStemmer()
    for ((line, sentenceId) <- source.getLines().zipWithIndex) {
      // First pass crease an ID-based graph.
      val (tokens, rawGraph) = parseFunction(line)
      // Second pass builds a lemmatized graph with edges holding tokens.
      val parseGraph = rawGraph.tokenized(tokens map stemmer.lemmatizePostaggedToken)

      // Add all of the token edges.
      for (node <- parseGraph.vertices) {
        val id = Token.id(corpus, sentenceId, node.id + 1)
        val tokenVertex = outputGraph.addVertex(id)
        tokenVertex.addEdge("token:text", outputGraph.addStringLiteralVertex(node.string))
        tokenVertex.addEdge("token:lemma", outputGraph.addStringLiteralVertex(node.lemma))
        tokenVertex.addEdge("token:pos", outputGraph.addStringLiteralVertex(node.postag))
        val begin = node.token.offset
        tokenVertex.addEdge("token:begin", outputGraph.addIntLiteralVertex(begin))
        tokenVertex.addEdge("token:end",
          outputGraph.addIntLiteralVertex(begin + node.string.length))
      }

      // Add all of the basic (non-collapsed) dependencies.
      for (edge <- parseGraph.edges) {
        val headId = Token.id(corpus, sentenceId, edge.source.id + 1)
        val head = outputGraph.getVertex(headId)
        val tailId = Token.id(corpus, sentenceId, edge.dest.id + 1)
        val tail = outputGraph.getVertex(tailId)
        val label = "basic:" + edge.label
        outputGraph.addEdge(label, head, tail, label)
      }

      // Create the collapsed "dep" graph.
      val collapsedGraph = rawGraph.collapse.tokenized(tokens map stemmer.lemmatizePostaggedToken)
      for (edge <- collapsedGraph.edges) {
        val headId = Token.id(corpus, sentenceId, edge.source.id + 1)
        val head = outputGraph.getVertex(headId)
        val tailId = Token.id(corpus, sentenceId, edge.dest.id + 1)
        val tail = outputGraph.getVertex(tailId)
        val label = "dep:" + edge.label
        outputGraph.addEdge(label, head, tail, label)
      }
    }

    DependencyGraph.toTurtle(outputGraph, destination)
    outputGraph.shutdown()
  }
}
