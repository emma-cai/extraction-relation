package org.allenai.extraction.rdf

import scala.io.Source

/** Utility methods for tokens in our RDF format. */
object Token {
  /** Builds a token ID for our RDF schema.
    * @param corpus a string representing which corpus this token came out of
    * @param sentenceNumber the sentence number in the corpus, starting with 1
    * @param tokenNumber the token number in the token's sentence, starting with 1
    * @return the URL (id) for a token with the given properties
    */
  def id(corpus: String, sentenceNumber: Int, tokenNumber: Int): String = {
    s"http://aristo.allenai.org/id#${corpus}/${sentenceNumber}_${tokenNumber}"
  }

  /** Extracts the corpus name from a Source's description. This uses everything up until the last
    * `.` in the string.
    */
  def corpus(source: Source): String = {
    val lastDot = source.descr.lastIndexOf(".")
    if (lastDot > 0) {
      source.descr.substring(0, lastDot)
    } else {
      source.descr
    }
  }
}
