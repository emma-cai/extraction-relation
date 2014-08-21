package org.allenai.extraction.processors.dependencies

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

class QuestionRulesTest extends UnitSpec {
  "QuestionRules.longestSequence" should "handle empty input" in {
    val input = Array[Vertex]()
    val result: Array[Vertex] = QuestionRules.longestSequence(input)
    assert(result.isEmpty === true)
  }

  it should "handle single node input" in {
    val graph = new MemoryStoreSailGraph()
    val v1: Vertex = graph.addVertex("http://id#test/1_1")
    val input = Array[Vertex](v1)
    val result: Array[Vertex] = QuestionRules.longestSequence(input)
    assert(result === input)
    graph.shutdown()
  }

  it should "handle ordered input" in {
    val graph = new MemoryStoreSailGraph()
    val v1: Vertex = graph.addVertex("http://id#test/1_1")
    val v2: Vertex = graph.addVertex("http://id#test/1_2")
    val v3: Vertex = graph.addVertex("http://id#test/1_3")
    val input = Array[Vertex](v1, v2, v3)
    val result: Array[Vertex] = QuestionRules.longestSequence(input)
    assert(result === input)
    graph.shutdown()
  }

  it should "handle unordered input" in {
    val graph = new MemoryStoreSailGraph()
    val v1: Vertex = graph.addVertex("http://id#test/1_1")
    val v2: Vertex = graph.addVertex("http://id#test/2_2")
    val v3: Vertex = graph.addVertex("http://id#test/3_3")
    val input = Array[Vertex](v1, v2, v3)
    val result: Array[Vertex] = QuestionRules.longestSequence(input)
    assert(result === Array[Vertex](v1))
    graph.shutdown()
  }

  it should "handle partially ordered input" in {
    val graph = new MemoryStoreSailGraph()
    val v1: Vertex = graph.addVertex("http://id#test/1_1")
    val v2: Vertex = graph.addVertex("http://id#test/2_2")
    val v3: Vertex = graph.addVertex("http://id#test/2_3")
    val input = Array[Vertex](v1, v2, v3)
    val result: Array[Vertex] = QuestionRules.longestSequence(input)
    assert(result === Array[Vertex](v2, v3))
    graph.shutdown()
  }
}
