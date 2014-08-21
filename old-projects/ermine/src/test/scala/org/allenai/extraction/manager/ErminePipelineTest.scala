package org.allenai.extraction.manager

import org.allenai.common.testkit.UnitSpec
import org.allenai.extraction.ErmineException

import com.typesafe.config.ConfigFactory

import scala.io.Source

import java.io.File
import java.io.StringWriter

class ErminePipelineTest extends UnitSpec {

  "ErminePipeline.fromConfig" should "parse a simple pipeline correctly" in {
    // Simplest valid pipeline.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)

    pipeline.name should be("TestWorkflow")
    pipeline.processors.length should be(1)
    pipeline.processors(0).processor should be(NoOpProcessor)
  }

  it should "parse a two-stage pipeline correctly" in {
    // Depth two pipeline.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
        },
        {
          name = "NoOpProcessor"
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)

    pipeline.processors.length should be(2)
    pipeline.processors(0).processor should be(NoOpProcessor)
    pipeline.processors(1).processor should be(NoOpProcessor)
  }

  it should "silently ignore extra outputs" in {
    // Depth two pipeline, with two outputs feeding into one input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "TwoByCatProcessor"
        },
        {
          name = "NoOpProcessor"
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    pipeline.requiredNamedInputs should be(Set())
  }

  it should "allow named outputs to be used for unnamed inputs" in {
    // Depth two pipeline, with two named outputs feeding into two unnamed inputs.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "TwoByCatProcessor"
          outputs = [ "one", "two" ]
        },
        {
          name = "TwoByCatProcessor"
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    pipeline.requiredNamedInputs should be(Set())
  }

  it should "fail when given a pipeline with one output followed by two unnamed inputs" in {
    // TwoBy expects two inputs, but NoOp just yields one.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
        },
        {
          name = "TwoByCatProcessor"
        }
      ]
      """)
    an[ErmineException] should be thrownBy {
      ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    }
  }

  it should "have one required input when given a pipeline with one unsatisfied input" in {
    // TwoBy expects two inputs, but NoOp just yields one.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          inputs = [ "foo" ]
          name = "NoOpProcessor"
        },
        {
          name = "NoOpProcessor"
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    pipeline.requiredNamedInputs should be(Set("foo"))
  }

  it should "have two required inputs when both stages have an unsatisfied input" in {
    // Both processors require named inputs from outside.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
          inputs = [ "bar" ]
        },
        {
          name = "NoOpProcessor"
          inputs = [ "foo" ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    pipeline.requiredNamedInputs should be(Set("foo", "bar"))
  }

  it should "handle named IO skipping stages" in {
    // First processor creates two outputs, each consumed by one input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "TwoByCatProcessor"
          outputs = [ "foo", "bar" ]
        },
        {
          name = "NoOpProcessor"
          inputs = [ "bar" ]
        },
        {
          name = "NoOpProcessor"
          inputs = [ "foo" ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    pipeline.requiredNamedInputs should be(Set())
  }

  it should "fail when given a mixture of anonymous first-stage input and named input" in {
    // First pipeline has unnamed input, second pipeline has missing named input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "TwoByCatProcessor"
        },
        {
          name = "NoOpProcessor"
          inputs = [ "bar" ]
        }
      ]
      """)
    an[ErmineException] should be thrownBy {
      ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    }
  }

  "ErminePipeline.run" should "handle piping outputs through a two-stage pipeline" in {
    // First processor creates two outputs, each consumed by one input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "TwoByCatProcessor"
          outputs = [ "foo", "bar" ]
        },
        {
          name = "CatProcessor"
          inputs = [ "bar" ]
        },
        {
          name = "CatProcessor"
          inputs = [ "foo" ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    val inputs = Seq(Source.fromString("fooey"), Source.fromString("barry"))
    val output = new StringWriter()

    // Run the pipeline, and assert that the "foo" input was piped to the output.
    pipeline.run(Map.empty, inputs, Seq(output))

    output.toString should be("fooey\n")
  }

  it should "validate input, and succeed on valid files" in {
    val file = File.createTempFile("test", ".txt")
    // First processor has a bad file input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
          inputs = [ { uri: "file://${file.getPath}" } ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    val inputs = Seq(Source.fromString(""))
    val outputs = Seq(new StringWriter())

    // No exceptions thrown.
    pipeline.run(Map.empty, inputs, outputs)
    file.delete()
  }

  it should "validate input, and fail on bad files" in {
    val file = File.createTempFile("test", ".txt")
    file.delete()
    // First processor has a bad file input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
          inputs = [ { uri: "file://${file.getPath}" } ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    val inputs = Seq(Source.fromString(""))
    val outputs = Seq(new StringWriter())

    an[ErmineException] should be thrownBy {
      pipeline.run(Map.empty, inputs, outputs)
    }
  }

  it should "validate outputs, and succeed on valid files" in {
    val file = File.createTempFile("test", ".txt")
    // First processor has a bad file input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
          outputs = [ { uri: "file://${file.getPath}" } ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    val inputs = Seq(Source.fromString(""))
    val outputs = Seq(new StringWriter())

    // No exceptions thrown.
    pipeline.run(Map.empty, inputs, outputs)
    file.delete()
  }

  it should "validate inputs, and fail on invalid files" in {
    val file = File.createTempFile("test", ".txt")
    file.delete()
    // First processor has a bad file input.
    val simpleConfig = ConfigFactory.parseString(s"""
      name = "TestWorkflow"
      processors = [
        {
          name = "NoOpProcessor"
          inputs = [ { uri: "file://${file.getPath}" } ]
        }
      ]
      """)
    val pipeline = ErminePipeline.fromConfig(simpleConfig)(TestErmineModule)
    val inputs = Seq(Source.fromString(""))
    val outputs = Seq(new StringWriter())

    an[ErmineException] should be thrownBy {
      pipeline.run(Map.empty, inputs, outputs)
    }
  }
}
