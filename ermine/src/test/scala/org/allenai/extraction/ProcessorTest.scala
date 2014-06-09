package org.allenai.extraction

import org.allenai.common.testkit.UnitSpec

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito.verify

import scala.io.Source

class ProcessorTest extends UnitSpec with MockitoSugar {
  object ConcreteProcessor extends Processor {
    override val numInputs = 2
    override val numOutputs = 1
    override protected def processInternal(sources: Seq[Source],
      destinations: Seq[Processor.Output]): Unit = {

      sources(1).getLines
      destinations(0).getOutputFile
    }
  }

  "A processor" should "delegate to the provided source and output" in {
    val usedSource = mock[Source]
    val unusedSource = mock[Source]
    val mockOutput = mock[Processor.Output]

    ConcreteProcessor.process(Seq(unusedSource, usedSource), Seq(mockOutput))

    verify(usedSource).getLines
    verify(mockOutput).getOutputFile
  }

  it should "not allow too many sources" in {
    val mockSource = mock[Source]
    val mockOutput = mock[Processor.Output]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockSource, mockSource, mockSource), Seq(mockOutput))
    }
  }
  it should "not allow too many writers" in {
    val mockSource = mock[Source]
    val mockOutput = mock[Processor.Output]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockSource, mockSource), Seq(mockOutput, mockOutput))
    }
  }
  it should "not allow too few sources" in {
    val mockSource = mock[Source]
    val mockOutput = mock[Processor.Output]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockSource), Seq(mockOutput))
    }
  }
  it should "not allow too few outputs" in {
    val mockSource = mock[Source]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockSource, mockSource), Seq.empty)
    }
  }
}
