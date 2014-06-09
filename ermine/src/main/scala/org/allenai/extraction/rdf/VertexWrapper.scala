package org.allenai.extraction.rdf

import com.tinkerpop.blueprints.Vertex

import scala.util.matching.Regex


// wrap Vertex to add helper methods
object VertexWrapper {
  implicit class VertexRdf (val v: Vertex) extends AnyVal {


    // format vertex name for use in SPARQL queries
    def toUri: String = {
      v.toString.stripPrefix("v[").stripSuffix("]")
    }

    // format vertex name for use as a label
    def toLiteral: String = {
      v.toUri.stripPrefix("\"").stripSuffix("\"")
    }

    // extract final number
    def tokenId: Int = {
      lazy val tokenIdPattern: Regex = """(?<=_)\d+(?=])""".r
      tokenIdPattern.findFirstIn(v.toString).map(_.toInt).getOrElse(0)
    }

    // for sorting by token number
    def <(v2: Vertex): Boolean = {
      v.tokenId < v2.tokenId
    }
  }
}
