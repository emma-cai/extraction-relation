package org.allenai.extraction.processors

import org.allenai.extraction.MultiTextProcessor
import org.allenai.extraction.rdf.DependencyGraph
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.clearnlp.dependency.{ DEPLib, DEPNode, DEPTree }
import com.clearnlp.nlp.{ NLPGetter, NLPMode }
import com.clearnlp.reader.AbstractReader
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source

import java.io.Writer

object ClearSrl extends MultiTextProcessor {
  /** The list of clear components we run, in order, to get SRL output. */
  lazy val clearComponents = Seq(
    NLPGetter.getComponent("general-en", AbstractReader.LANG_EN, NLPMode.MODE_MORPH),
    NLPGetter.getComponent("general-en", AbstractReader.LANG_EN, NLPMode.MODE_PRED),
    NLPGetter.getComponent("general-en", AbstractReader.LANG_EN, NLPMode.MODE_ROLE),
    NLPGetter.getComponent("general-en", AbstractReader.LANG_EN, NLPMode.MODE_SRL))

  override def processText(source: Source, destination: Writer): Unit = {
    // Load the stanford dependency tree.
    val graph = new MemoryStoreSailGraph()
    DependencyGraph.fromTurtle(graph, source)

    // Select all of the tokens in the tree.
    val query = """SELECT ?id ?text ?pos ?begin WHERE {
        ?id token:text ?text .
        ?id token:pos ?pos .
      }"""
    val results = DependencyGraph.executeSparql(graph, query)
    // Group the tokens by sentence ID.
    val sentences = mutable.Map.empty[Int, Seq[(Vertex, DEPNode)]]
    for (result <- results) {
      val vertex = result("id")

      // Create a new node with the data from the result.
      // Note that the zero-argument DEPNode constructor results in an uninitialized DEPNode, and
      // shouldn't be used.
      val newNode = new DEPNode(vertex.tokenId, result("text").toStringLiteral)
      newNode.pos = result("pos").toStringLiteral

      // Add the vertex & node to the list for the sentence.
      val sentenceId = vertex.sentenceId
      val nodes = sentences.getOrElse(sentenceId, Seq.empty)
      sentences.put(sentenceId, nodes :+ ((vertex, newNode)))
    }

    // Create a dependency tree for each sentence, and run the clear processors over it.
    for ((sentenceId, vertexNodePairs) <- sentences) {
      // Sort the nodes / vertices by ID so that we can easily retrieve them.
      val (sortedVertices, sortedNodes) = (vertexNodePairs sortBy { pair => pair._2.id }).unzip
      // We start numbering at 1 - if there are any gaps / duplicates, our logic below will fail.
      require(sortedNodes.last.id == sortedNodes.size, "Missing or duplicate tokens in graph!")

      // Tree (with root node by default).
      val tree = new DEPTree()
      val rootNode = tree.get(0)

      // Add all of the nodes, in token-ID order (so we can retrieve them again).
      tree.ensureCapacity(sortedNodes.size)
      for (node <- sortedNodes) {
        tree.add(node)
      }

      // Add all of the stanford dependencies.
      for ((vertex, node) <- vertexNodePairs) {
        // Get all of the basic dependencies for the given node from our graph.
        val query = s"""SELECT ?head ?dep WHERE {
            ?head ?dep <${vertex.toUri}> .
            FILTER regex(str(?dep), "http://nlp.stanford.edu/basic")
          }"""
        val results = DependencyGraph.executeSparql(graph, query)
        for (result <- results) {
          val headId = result("head").tokenId
          val head = tree.get(headId)
          require(head != null, "Head not in tree: " + headId)
          node.setHead(head, result("dep").lastPathElement)
        }
        // Default to a dependency on the root of the tree.
        if (results.size == 0) {
          node.setHead(rootNode, "root")
        }
      }

      // Run the clear components over the tree.
      for (component <- clearComponents) {
        component.process(tree)
      }

      // Pull out the SRL frames.
      for {
        node <- sortedNodes
        srlArc <- node.getSHeads().asScala
      } {
        val srlHead = srlArc.getNode
        val srlLabel = srlArc.getLabel

        // Argument arcs also have functions (`srlArc.getFunctionTag`). We currently ignore these.
        // Head nodes also have a "name.sense" string (stored in `srlHead.getFeat(DEPLib.FEAT_PB)`.
        // These are likewise ignored - `name` seems to duplicate the verb lemma, while `sense`
        // looks like an index into propbank word senses.

        // Make an edge from `srlHead` to `node`, with `label` as the ID.
        val headVertex = sortedVertices(srlHead.id - 1)
        val tailVertex = sortedVertices(node.id - 1)
        headVertex.addEdge(s"http://clearnlp.com/srl/${srlLabel}", tailVertex)
      }
    }

    DependencyGraph.toTurtle(graph, destination)
    graph.shutdown()
  }
}
