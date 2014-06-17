package org.allenai.extraction.rdf

import org.allenai.common.SourceInputStream
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.sail.SailGraph

import scala.io.Source
import scala.collection.JavaConverters._

import java.io.Writer
import java.nio.charset.StandardCharsets
import org.apache.commons.io.output.WriterOutputStream

object DependencyGraph {
  /** Create a new graph from a TTL source. */
  def fromTurtle(graph: SailGraph, source: Source): SailGraph = {
    graph.loadRDF(new SourceInputStream(source), "http://aristo.allenai.org", "turtle", null)
    graph
  }

  /** Write a graph out as TTL to the given writer. */
  def toTurtle(graph: SailGraph, sink: Writer) = {
    val sinkStream = new WriterOutputStream(sink, StandardCharsets.UTF_8)
    graph.saveRDF(sinkStream, "turtle")
  }

  /** Executes the given sparql query, returning the results as scala objects. */
  def executeSparql(graph: SailGraph, query: String): Seq[Map[String, Vertex]] = {
    val result: java.util.List[java.util.Map[String, Vertex]] = graph.executeSparql(query)
    // convert to Scala
    for {
      javaMap <- result.asScala.toSeq
    } yield javaMap.asScala.toMap
  }
}
