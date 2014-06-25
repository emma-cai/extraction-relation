package org.allenai.extraction.rdf

import org.allenai.extraction.ErmineException

import com.tinkerpop.blueprints.Vertex

/** Wrap Vertex to add helper methods. */
object VertexWrapper {
  val SentenceTokenIds = """(\d+)_(\d+)""".r
  val LastPath = """/([^/]+)/?$""".r
  implicit class VertexRdf(val v: Vertex) extends AnyVal {
    /** Returns the string value, if a literal; or the String form of the ID, if a URI. Useful for
      * adding edges when you're unsure of the starting type.
      */
    def toIdString: String = v.getProperty[String]("kind") match {
      case "literal" => v.getProperty[String]("value")
      case "uri" => v.getId.toString
      case _ => throw new ErmineException(s"attempted to get a string id from a blank node: ${v}")
    }

    // format vertex name for use in SPARQL queries
    def toUri: String = v.getProperty[String]("kind") match {
      case "uri" => v.getId.toString
      case _ => throw new ErmineException(s"attempted to get a uri from a non-uri vertex ${v}")
    }

    def lastPathElement: String = LastPath findFirstIn v.toUri match {
      case Some(LastPath(path)) => path
      case _ => throw new ErmineException(s"malformed uri in vertex ${v}")
    }

    // format vertex name for use as a label
    def toStringLiteral: String = v.getProperty[String]("kind") match {
      case "literal" => v.getProperty[String]("value")
      case _ =>
        throw new ErmineException(s"attempted to get a literal from a non-literal vertex ${v}")
    }

    def toIntLiteral: Int = v.getProperty[String]("kind") match {
      // We need to fetch the Java Integer wrapper here; it gets converted with an implicit to the
      // scala wrapper.
      case "literal" => v.getProperty[Integer]("value")
      case _ =>
        throw new ErmineException(s"attempted to get a literal from a non-literal vertex ${v}")
    }

    def ids: (Int, Int) = SentenceTokenIds findFirstIn v.toUri match {
      case Some(SentenceTokenIds(sentenceId, tokenId)) => (sentenceId.toInt, tokenId.toInt)
      case _ => throw new ErmineException(s"malformed uri in vertex ${v}")
    }

    def sentenceId: Int = v.ids._1

    def tokenId: Int = v.ids._2

    // for sorting by token number
    def <(v2: Vertex): Boolean = {
      v.tokenId < v2.tokenId
    }
  }
}
