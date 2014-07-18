package org.allenai.extraction.processors

import org.allenai.extraction.{ ErmineException, FlatProcessor }
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

      // The fake token we'll draw an edge from to the root node.
      val fakeRootToken = outputGraph.addVertex(Token.id(corpus, sentenceId, 0))

      // Add all of the basic (non-collapsed) dependencies.
      for (edge <- parseGraph.edges) {
        val headId = Token.id(corpus, sentenceId, edge.source.id + 1)
        val head = outputGraph.getVertex(headId)
        val tailId = Token.id(corpus, sentenceId, edge.dest.id + 1)
        val tail = outputGraph.getVertex(tailId)
        val label = "basic:" + edge.label
        outputGraph.addEdge(label, head, tail, label)
      }
      // Add the root of the basic graph.
      rawGraph.root match {
        case Some(node) => {
          val rootNode = outputGraph.getVertex(Token.id(corpus, sentenceId, node.id + 1))
          val label = "basic:root"
          outputGraph.addEdge(label, fakeRootToken, rootNode, label)
        }
        case None => throw new ErmineException("basic dependency graph has no roots!")
      }

      // Create the collapsed "dep" graph.
      val rawCollapsedGraph = rawGraph.collapse
      val collapsedGraph = rawCollapsedGraph.tokenized(tokens map stemmer.lemmatizePostaggedToken)
      for (edge <- collapsedGraph.edges) {
        val headId = Token.id(corpus, sentenceId, edge.source.id + 1)
        val head = outputGraph.getVertex(headId)
        val tailId = Token.id(corpus, sentenceId, edge.dest.id + 1)
        val tail = outputGraph.getVertex(tailId)
        val label = "dep:" + edge.label
        outputGraph.addEdge(label, head, tail, label)
      }
      // Add the root of the collapsed graph.
      rawCollapsedGraph.root match {
        case Some(node) => {
          val rootNode = outputGraph.getVertex(Token.id(corpus, sentenceId, node.id + 1))
          val label = "dep:root"
          outputGraph.addEdge(label, fakeRootToken, rootNode, label)
        }
        case None => throw new ErmineException("collapsed dependency graph has no roots!")
      }
    }

    DependencyGraph.toTurtle(outputGraph, destination)
    outputGraph.shutdown()
  }
}
