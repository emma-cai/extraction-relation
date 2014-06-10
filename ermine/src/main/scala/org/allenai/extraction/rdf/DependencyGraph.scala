package org.allenai.extraction.rdf

import org.allenai.extraction.manager.io.SourceInputStream
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source
import scala.collection.JavaConverters._

import java.io.Writer
import java.nio.charset.StandardCharsets
import org.apache.commons.io.output.WriterOutputStream


/** wrapper class to support dependency-specific operations on an rdf graph */
class DependencyGraph extends MemoryStoreSailGraph {

  setNamespaces(this)

  def setNamespaces(graph: MemoryStoreSailGraph) = {
    graph.addDefaultNamespaces // rdf: rdfs:
    graph.addNamespace("id","http://aristo.allenai.org/id#")
    graph.addNamespace("token","http://nlp.stanford.edu/token/")
    graph.addNamespace("ne","http://nlp.stanford.edu/ne/")
    graph.addNamespace("basic","http://nlp.stanford.edu/basic/")
    graph.addNamespace("dep","http://nlp.stanford.edu/dep/")
    graph.addNamespace("rel","http://aristo.allenai.org/rel/")
    graph.addNamespace("pred","http://aristo.allenai.org/pred/")
  }

  /** graph where all output will be written */
  val outputGraph: MemoryStoreSailGraph = {
    val graph = new MemoryStoreSailGraph()
    setNamespaces(graph)
    graph
  }

  override def shutdown() = {
    outputGraph.shutdown()
    super.shutdown()
  }

  def loadTurtle(source: Source) = {
    val sourceStream = new SourceInputStream(source, "UTF-8")
    loadRDF(sourceStream, "http://aristo.allenai.org", "turtle", null)
  }

  def saveTurtle(sink: Writer) = {
    val sinkStream = new WriterOutputStream(sink, StandardCharsets.UTF_8)
    saveRDF(sinkStream, "turtle") // original input
    outputGraph.saveRDF(sinkStream, "turtle") // any additions
  }

  /** wrap SailGraph.executeSparql */
  def executeQuery(query: String): Seq[Map[String, Vertex]] = {
    val result: java.util.List[java.util.Map[String,Vertex]] = super.executeSparql(query)
    // convert to Scala
    for {
      javaMap <- result.asScala.toSeq
    } yield javaMap.asScala.toMap
  }

}

