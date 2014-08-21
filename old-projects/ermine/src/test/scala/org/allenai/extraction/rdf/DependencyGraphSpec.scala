package org.allenai.extraction.rdf

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf
import org.scalatest.BeforeAndAfterAll
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

import scala.io.Source

/** Tests DependencyGraph helpers. */
class DependencyGraphSpec extends UnitSpec with BeforeAndAfterAll {
  /** Simple graph fixture. */
  val graph = new MemoryStoreSailGraph()
  override def beforeAll() = {
    super.beforeAll()
    DependencyGraph.fromTurtle(graph, Source.fromString("""
@prefix id: <http://aristo.allenai.org/id#> .
@prefix rel: <http://aristo.allenai.org/rel/> .

id:1 rel:two id:2 .
id:1 rel:three id:3 .
"""))
  }
  override def afterAll() = {
    super.afterAll()
    graph.rollback()
    graph.shutdown()
  }

  "DependencyGraph.executeSparql" should "populate a Scala collection with the results" in {
    val results = DependencyGraph.executeSparql(graph, "SELECT ?x ?y WHERE { ?x rel:two ?y }")
    assert(results.size === 1)
    assert(results(0)("x").toUri === "http://aristo.allenai.org/id#1")
    assert(results(0)("y").toUri === "http://aristo.allenai.org/id#2")
  }

  "DependencyGraph.copy" should "populate an empty graph" in {
    val newGraph = new MemoryStoreSailGraph()
    DependencyGraph.copy(graph, newGraph)

    // Assert both edges were added.
    assert(newGraph.executeSparql("SELECT ?x WHERE { ?x ?rel ?y }").size === 2)
    // Assert that the rel:two edge is in the correct direction (to id 2).
    val relOne = newGraph.executeSparql(
      "SELECT ?x ?y WHERE { ?x <http://aristo.allenai.org/rel/two> ?y }")
    assert(relOne.size === 1)
    assert(relOne.get(0).get("x").toUri === "http://aristo.allenai.org/id#1")
    assert(relOne.get(0).get("y").toUri === "http://aristo.allenai.org/id#2")
  }

  it should "populate a non-empty graph" in {
    val newGraph = new MemoryStoreSailGraph()
    DependencyGraph.fromTurtle(newGraph, Source.fromString("""
@prefix id: <http://aristo.allenai.org/id#> .
@prefix rel: <http://aristo.allenai.org/rel/> .

# One duplicate relationship, one new.
id:1 rel:two id:2 .
id:2 rel:three id:3 .
"""))
    DependencyGraph.copy(graph, newGraph)

    // Assert both edges were added.
    assert(newGraph.executeSparql("SELECT ?x WHERE { ?x ?rel ?y }").size === 3)
    // Assert that the rel:three edge is in the correct direction.
    val newRelTwo = newGraph.executeSparql("SELECT ?y WHERE { id:1 rel:three ?y }")
    assert(newRelTwo.size === 1)
    assert(newRelTwo.get(0).get("y").toUri === "http://aristo.allenai.org/id#3")
    // Assert that the old relation was retained.
    val oldRelTwo = newGraph.executeSparql("SELECT ?y WHERE { id:2 rel:three ?y }")
    assert(oldRelTwo.size === 1)
    assert(oldRelTwo.get(0).get("y").toUri === "http://aristo.allenai.org/id#3")
  }
}
