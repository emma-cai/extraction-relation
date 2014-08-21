package org.allenai.extraction

import org.allenai.common.testkit.UnitSpec

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito.verify

class ProcessorTest extends UnitSpec with MockitoSugar {
  object ConcreteProcessor extends Processor {
    override val numInputs = 2
    override val numOutputs = 1
    override protected def processInternal(sources: Seq[Processor.Input],
      destinations: Seq[Processor.Output]): Unit = {

      sources(1).getSources
      destinations(0).getOutputFile
    }
  }

  "A processor" should "delegate to the provided input and output" in {
    val usedInput = mock[Processor.Input]
    val unusedInput = mock[Processor.Input]
    val mockOutput = mock[Processor.Output]

    ConcreteProcessor.process(Seq(unusedInput, usedInput), Seq(mockOutput))

    verify(usedInput).getSources
    verify(mockOutput).getOutputFile
  }

  it should "not allow too many inputs" in {
    val mockInput = mock[Processor.Input]
    val mockOutput = mock[Processor.Output]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockInput, mockInput, mockInput), Seq(mockOutput))
    }
  }
  it should "not allow too many outputs" in {
    val mockInput = mock[Processor.Input]
    val mockOutput = mock[Processor.Output]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockInput, mockInput), Seq(mockOutput, mockOutput))
    }
  }
  it should "not allow too few inputs" in {
    val mockInput = mock[Processor.Input]
    val mockOutput = mock[Processor.Output]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockInput), Seq(mockOutput))
    }
  }
  it should "not allow too few outputs" in {
    val mockInput = mock[Processor.Input]

    an[IllegalArgumentException] should be thrownBy {
      ConcreteProcessor.process(Seq(mockInput, mockInput), Seq.empty)
    }
  }
}
