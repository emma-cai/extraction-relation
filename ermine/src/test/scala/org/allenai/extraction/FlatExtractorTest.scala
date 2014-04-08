package org.allenai.extraction

import org.allenai.common.testkit.UnitSpec

import org.scalatest.mock.MockitoSugar

import org.mockito.Mockito.verify

import scala.io.Source

import java.io.Writer

class FlatExtractorTest extends UnitSpec with MockitoSugar {
  object ConcreteExtractor extends FlatExtractor {
    override protected def extractInternal(source: Source, destination: Writer): Unit = {
      source.getLines
      destination.write("foo")
    }
  }

  "A flat extractor" should "delegate to the provided source and writer" in {
    val mockSource = mock[Source]
    val mockWriter = mock[Writer]

    ConcreteExtractor.extract(Seq(mockSource), Seq(mockWriter))

    verify(mockSource).getLines
    verify(mockWriter).write("foo")
  }

  it should "not allow more than one source" in {
    val mockSource = mock[Source]
    val mockWriter = mock[Writer]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource, mockSource), Seq(mockWriter))
    }
  }
  it should "not allow more than one writer" in {
    val mockSource = mock[Source]
    val mockWriter = mock[Writer]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource), Seq(mockWriter, mockWriter))
    }
  }
  it should "not allow zero sources" in {
    val mockWriter = mock[Writer]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq.empty, Seq(mockWriter))
    }
  }
  it should "not allow zero writers" in {
    val mockSource = mock[Source]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteExtractor.extract(Seq(mockSource), Seq.empty)
    }
  }
}
