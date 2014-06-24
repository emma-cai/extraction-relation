package org.allenai.extraction.rdf

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf
import org.scalatest.BeforeAndAfterAll
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

/** Tests VertexRdf magic. */
class VertexWrapperSpec extends UnitSpec with BeforeAndAfterAll {
  /** Graph with URI IDs, a string literal, and an int literal. */
  val graph = new MemoryStoreSailGraph()
  override def beforeAll() = {
    super.beforeAll()
    DependencyGraph.fromTurtle(graph, Source.fromString("""
@prefix id: <http://aristo.allenai.org/id/barrons/> .
@prefix rel: <http://aristo.allenai.org/rel/> .

id:1_2 rel:one "string literal" .
id:1_2 rel:two 123 .
"""))
  }
  override def afterAll() = {
    super.afterAll()
    graph.shutdown()
  }

  "A wrapped vertex" should "return a valid URI" in {
    val results =
      DependencyGraph.executeSparql(graph, """SELECT ?id WHERE { ?id ?rel "string literal" }""")
    assert(results(0)("id").toUri === "http://aristo.allenai.org/id/barrons/1_2")
  }

  it should "return a valid string literal" in {
    val results =
      DependencyGraph.executeSparql(graph, """SELECT ?literal WHERE { ?id rel:one ?literal }""")
    assert(results(0)("literal").toStringLiteral === "string literal")
  }

  it should "return a valid integer literal" in {
    val results =
      DependencyGraph.executeSparql(graph, """SELECT ?literal WHERE { ?id rel:two ?literal }""")
    assert(results(0)("literal").toIntLiteral === 123)
  }

  it should "return a proper path ending" in {
    val results =
      DependencyGraph.executeSparql(graph, """SELECT ?rel WHERE { ?id ?rel "string literal" }""")
    assert(results(0)("rel").lastPathElement === "one")
  }

  it should "return valid sentence and token IDs" in {
    val results =
      DependencyGraph.executeSparql(graph, """SELECT ?id WHERE { ?id ?rel "string literal" }""")
    val idVertex = results(0)("id")
    assert(idVertex.ids === ((1, 2)))
    assert(idVertex.sentenceId === 1)
    assert(idVertex.tokenId === 2)
  }
}
