# Extraction

This project is meant to contain glue code that integrates our various extraction processes into a single pipeline.

## Project layout

There are three subprojects:

1. api: A definition of the API to talk to the Ermine service
2. demo: A webapp running an extraction demo
3. ermine: A manager to handle data pipelines
4. service: A webservice fronting ermine

# Ermine

"Ermine" is the name of the extraction management system. At its core, it's a
workflow management system - a way to plug output from one text processor into
the input for another, with a tiny bit of AI2-specific glue.

The "ermine" subproject contains the core engine for running pipelines, as well
as (currently) having the code for the pipeline stages themselves. The
"service" directory contains an HTTP frontend for Ermine, running a
configurable set of pipelines.

If you're running an extraction pipeline offline - not as a part of a Ari solver - you'll want to use
the commandline Ermine version.

## Creating an Ermine pipeline

Each ermine execution consists of a single *pipeline*, which is an ordered list of *processors*. A pipeline specifies the order to execute the processors, and what their inputs and outputs are. Each processor can read one or more inputs, and write one or more outputs, determined by their implementation. What ermine does is connect the inputs to the outputs, and save them to a configured location.

If you're building an extractor, think about it in terms of processing streams:
Does it operate on a single text file, line-by-line, and produce
JSON-serializable objects? Does it take a question & focus, and produce an Arilog output of that
question?

### Processors

As a first step, you should decompose your problem into useful substeps with
I/O, and map each of these steps to a processor.  For example, we have
(a processor that runs the Stanford dependency parser)[https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/StanfordParser.scala], and streams out the
dependency parse as XML - and (a separate processor to convert that XML)[https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/StanfordXmltoTtl.scala] to the TTL graph
format. This way, we both have two smaller problems to solve - and someone
could reuse the Stanford processor down the line.

Processors should extend the [Processor](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/Processor.scala). We've been putting processors in [extraction/processors](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/CatProcessor.scala).
For a simple example processor, check out [CatProcessor](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/CatProcessor.scala) (named after the Linux tool, not the furry mammal).

Once written, the processor needs to be added to the processor configuration map, which is created in [ErmineModule](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/manager/ErmineModule.scala#L24). Ideally, we could look up processors at runtime by class name . . . but this is not very reliable with the current Scala reflection libraries.

### Pipelines

Once you have processors in place, you're ready to write a pipeline. A pipeline is configured in a
typesafe config file. By default, Ermine locally looks under the `ermine.pipeline` config key, but you can change this with the `-p` commandline flag.

A pipeline has the following fields:
* `name`: A human-readable name of the pipeline. Required.
* `description`: A longer description of the pipeline. Optional.
* `processors`: An array of processors to run. Required, and must be non-empty.

The processors in a pipeline are always run in the order configured.

A processor has the following fields:
* `name`: The name of the processor to use. Required. Must map to a processor implementation in [this map](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/manager/ErmineModule.scala#L24).
* `inputs`: Array of inputs. Optional.
* `outputs`: Array of outputs. Optional.

Each processor in the pipeline must have its inputs and outputs satisfied. The number and order of
these is fixed by the implementation; where they read from and write to is up
to the pipeline configuration to specify.

Each processor handles input as follows:
1. If the inputs list is missing and the processor is the first in the pipeline, the user must provide the inputs as command-line arguments.
2. If the inputs list is missing and the processor is not the first in the pipeline, then it will use the outputs of the previous processor as input. 
3. If the inputs list is present, the processor will look for named inputs to the pipeline (if run through the Ermine service) and / or named outputs of previous pipeline stages for streams matching the configured names, and use the contents of those.

It is considered a configuration error if:
* A processor with no inputs configured follows a processor with too few outputs to satisfy it.
* The first processor requires command-line inputs, but a later processor requires named pipeline inputs. *NOTE*: This is mostly a failing of the Ermine interface; the ability to name input streams on the commandline is a project TODO.

Normally, inputs and outputs only need to be simple strings. If desired, you may also add a `uri` to the input configuration to pipe the results to another location. They will still be available for downstream processors; this is similar to using the `tee` linux utility.

[The Ferret example](https://github.com/allenai/extraction/blob/master/ermine/examples/ferret.conf) is a good place to look for a sample configuration.

## Running ermine

The main ermine project can be built without any special configuration, although you do need Prolog
installed to run the Ferret pipelines - see below.

After installing prolog, you can run `sbt 'project ermine' stage` to build ermine.

The auto-generated script `ermine/target/universal/stage/bin/extraction-manager` can then be used to run a pipeline:

`./ermine/target/universal/stage/bin/extraction-manager -c ermine/examples/ferret.conf -i some-sentences-file.txt -o output.ttl`

The `-c` flag is the only required one, and it specifies the configuration file for the pipeline you're running. the `-i` and `-o` flags are only used if your pipeline doesn't specify a first-stage input or a last-stage output (respectively). `-i` can be repeated if you have multiple inputs to the start of your pipeline.


### Installing Prolog

The Ferret processors currently depend on having swipl Prolog installed with the JPL library.

You can install this easily on OS X with [homebrew](http://brew.sh/):

`brew install swi-prolog --with-jpl`

Don't forget the `--with-jpl` flag! The root project's `Build.scala` will check for a valid swipl install at load time, and will print out a warning if one is missing.

