package org.allenai.extraction.rdf

import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import java.io.FileInputStream
import scala.collection.JavaConverters._


class DependencyGraph extends MemoryStoreSailGraph {

  /** wrap SailGraph.executeSparql */
  def executeQuery(query: String): Seq[Map[String, Vertex]] = {
    val result: java.util.List[java.util.Map[String,Vertex]] = super.executeSparql(query)
    // convert to Scala
    for {
      javaMap <- result.asScala.toSeq
    } yield javaMap.asScala.toMap
  }

}

