package org.allenai.extraction.processors

/** Object holding helper classes for TTL output. */
object Ttl {
  /** Namespace headers for our TTL files. */
  val NamespaceHeaders = """
@prefix id: <http://aristo.allenai.org/id#> .

@prefix token: <http://nlp.stanford.edu/token/> .
@prefix ne: <http://nlp.stanford.edu/ne/> .
@prefix basic: <http://nlp.stanford.edu/basic/> .
@prefix dep: <http://nlp.stanford.edu/dep/> .
@prefix coref: <http://nlp.stanford.edu/coref/> .

"""
}
