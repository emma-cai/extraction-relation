package org.allenai.extraction.rdf

import org.allenai.common.SourceInputStream
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.{ Direction, Vertex }
import com.tinkerpop.blueprints.impls.sail.SailGraph

import scala.io.Source
import scala.collection.JavaConverters._

import java.io.Writer
import java.nio.charset.StandardCharsets
import org.apache.commons.io.output.WriterOutputStream

object DependencyGraph {
  /** Load graph content from a TTL source. */
  def fromTurtle(graph: SailGraph, source: Source): SailGraph = {
    graph.loadRDF(new SourceInputStream(source), "http://aristo.allenai.org", "turtle", null)
    graph
  }

  /** Write a graph out as TTL to the given writer. */
  def toTurtle(graph: SailGraph, sink: Writer) = {
    val sinkStream = new WriterOutputStream(sink, StandardCharsets.UTF_8)
    graph.saveRDF(sinkStream, "turtle")
  }

  /** Copies one RDF graph into another.
    * @param source the input graph
    * @param dest the destination graph to copy into
    * @return a reference to `dest`
    */
  def copy(source: SailGraph, dest: SailGraph): SailGraph = {
    for (edge <- source.getEdges.asScala) {
      dest.addEdge(edge.getId, edge.getVertex(Direction.OUT), edge.getVertex(Direction.IN),
        edge.getLabel)
    }
    dest
  }

  /** Executes the given sparql query, returning the results as scala objects. */
  def executeSparql(graph: SailGraph, query: String): Seq[Map[String, Vertex]] = {
    val result: java.util.List[java.util.Map[String, Vertex]] = graph.executeSparql(query)
    // convert to Scala
    for {
      javaMap <- result.asScala.toSeq
    } yield javaMap.asScala.toMap
  }

  def setNamespaces(graph: SailGraph) = {
    graph.addDefaultNamespaces // rdf: rdfs:
    graph.addNamespace("id", "http://aristo.allenai.org/id#")
    graph.addNamespace("token", "http://nlp.stanford.edu/token/")
    graph.addNamespace("ne", "http://nlp.stanford.edu/ne/")
    graph.addNamespace("basic", "http://nlp.stanford.edu/basic/")
    graph.addNamespace("dep", "http://nlp.stanford.edu/dep/")
    graph.addNamespace("wn", "http://wordnet.princeton.edu/")
    graph.addNamespace("rel", "http://aristo.allenai.org/rel/")
    graph.addNamespace("pred", "http://aristo.allenai.org/pred/")
  }

  /** find conjuncts of a node */
  def conjoinedNodes(graph: SailGraph, node: Vertex): Seq[Vertex] = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?conj WHERE {
        { <$uri> dep:conj_and ?conj . }
        UNION
        { ?conj dep:conj_and <$uri> . }
      }"""
    val result: Seq[Map[String, Vertex]] = executeSparql(graph, query)
    result.headOption map { _.values.toSeq } getOrElse { Seq.empty }
  }

  /** collect all tokens below node in the basic dependency tree */
  def nodeConstits(graph: SailGraph, node: Vertex, exclude: Option[String] = None): Seq[Vertex] = {
    val uri: String = node.toUri
    val filter = exclude match {
      case Some(deps) => s"FILTER(!regex(str(?dep), '/($deps)$$')) ."
      case None => ""
    }
    val query: String = s"""
      SELECT ?constit WHERE {
        <$uri> ?dep ?constit .
        FILTER(STRSTARTS(str(?dep), "http://nlp.stanford.edu/basic/")) .
        $filter
      }"""
    val results: Seq[Map[String, Vertex]] = executeSparql(graph, query)
    val allValues: Seq[Seq[Vertex]] = for {
      map <- results
      constit = map("constit")
    } yield (constit +: nodeConstits(graph, constit)) // recurse without exclusions
    allValues.flatten
  }

  /** retrieve properties of a token
    * valid prop values in current Stanford output are:
    * - text
    * - lemma
    * - pos
    * - begin
    * - end
    */
  def tokenInfo(graph: SailGraph, node: Vertex, prop: String): String = {
    val uri: String = node.toUri
    val query: String = s"""
      SELECT ?prop WHERE {
        <$uri> token:$prop ?prop .
      }"""
    val result: Map[String, Vertex] = executeSparql(graph, query).head
    result("prop").toStringLiteral
  }

  /** Wrapper to add helper methods. */
  implicit class GraphRdf(val graph: SailGraph) extends AnyVal {
    /** Adds a string literal vertex to this graph. */
    def addStringLiteralVertex(string: String): Vertex = graph.addVertex('"' + string + '"')

    /** Adds an integer literal vertex to this graph. */
    def addIntLiteralVertex(int: Int): Vertex = {
      graph.addVertex(s""""${int}"^^<http://www.w3.org/2001/XMLSchema#integer>""")
    }
  }
}
