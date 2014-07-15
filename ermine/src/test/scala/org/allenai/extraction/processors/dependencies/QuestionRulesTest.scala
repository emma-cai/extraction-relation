package org.allenai.extraction.processors.dependencies

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.rdf.VertexWrapper.VertexRdf

import com.tinkerpop.blueprints.Vertex

class QuestionRulesTest extends UnitSpec {
  "QuestionRules.longestSequence" should "handle empty input" in {
    val processor = QuestionRules
    val input = Array[Vertex]()
    val result: Array[Vertex] = processor.longestSequence(input)
    assert(result.isEmpty === true)
  }

  it should "handle single node input" in {
    val processor = QuestionRules
    val v1: Vertex = processor.inputGraph.addVertex("http://1_1")
    val input = Array[Vertex](v1)
    val result: Array[Vertex] = processor.longestSequence(input)
    assert(result === input)
  }

  it should "handle ordered input" in {
    val processor = QuestionRules
    val v1: Vertex = processor.inputGraph.addVertex("http://1_1")
    val v2: Vertex = processor.inputGraph.addVertex("http://1_2")
    val v3: Vertex = processor.inputGraph.addVertex("http://1_3")
    val input = Array[Vertex](v1, v2, v3)
    val result: Array[Vertex] = processor.longestSequence(input)
    assert(result === input)
  }

  it should "handle unordered input" in {
    val processor = QuestionRules
    val v1: Vertex = processor.inputGraph.addVertex("http://1_1")
    val v2: Vertex = processor.inputGraph.addVertex("http://2_2")
    val v3: Vertex = processor.inputGraph.addVertex("http://3_3")
    val input = Array[Vertex](v1, v2, v3)
    val result: Array[Vertex] = processor.longestSequence(input)
    assert(result === Array[Vertex](v1))
  }

  it should "handle partially ordered input" in {
    val processor = QuestionRules
    val v1: Vertex = processor.inputGraph.addVertex("http://1_1")
    val v2: Vertex = processor.inputGraph.addVertex("http://2_2")
    val v3: Vertex = processor.inputGraph.addVertex("http://2_3")
    val input = Array[Vertex](v1, v2, v3)
    val result: Array[Vertex] = processor.longestSequence(input)
    assert(result === Array[Vertex](v2, v3))
  }
}
