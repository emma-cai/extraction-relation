package org.allenai.extraction

import org.allenai.common.testkit.UnitSpec

import org.scalatest.mock.MockitoSugar

import org.mockito.Mockito.verify

import scala.io.Source

import java.io.Writer

class ExtractorTest extends UnitSpec with MockitoSugar {
  object ConcreteExtractor extends Extractor {
    override val numInputs = 2 
    override val numOutputs = 1 
    override protected def extractInternal(sources: Seq[Source], destinations: Seq[Writer]):
        Unit = {
      sources(1).getLines
      destinations(0).write("foo")
    }
  }

  "An extractor" should "delegate to the provided source and writer" in {
    val usedSource = mock[Source]
    val unusedSource = mock[Source]
    val mockWriter = mock[Writer]

    ConcreteExtractor.extract(Seq(unusedSource, usedSource), Seq(mockWriter))

    verify(usedSource).getLines
    verify(mockWriter).write("foo")
  }

  it should "not allow too many sources" in {
    val mockSource = mock[Source]
    val mockWriter = mock[Writer]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource, mockSource, mockSource), Seq(mockWriter))
    }
  }
  it should "not allow too many writers" in {
    val mockSource = mock[Source]
    val mockWriter = mock[Writer]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource, mockSource), Seq(mockWriter, mockWriter))
    }
  }
  it should "not allow too few sources" in {
    val mockSource = mock[Source]
    val mockWriter = mock[Writer]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource), Seq(mockWriter))
    }
  }
  it should "not allow too few writers" in {
    val mockSource = mock[Source]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource, mockSource), Seq.empty)
    }
  }
}
