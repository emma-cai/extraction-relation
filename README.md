# Extraction

This project is meant to contain glue code that integrates our various extraction processes into a single pipeline.

Some demos are at [extraction.allenai.org](http://extraction.allenai.org/).

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
(a processor that runs the Stanford dependency parser)[https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/StanfordTtl.scala], and streams out the
dependency parse as TTL - and [a separate processor to run Clear SRL](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/ClearSrl.scala) on the TTL graph
format. This way, we both have two smaller problems to solve - and someone
could reuse the Stanford processor down the line.

Text-based processors should extend [TextProcessor](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/Processor.scala). We've been putting processors in [extraction/processors](https://github.com/allenai/extraction/blob/master/ermine/src/main/scala/org/allenai/extraction/processors/CatProcessor.scala).
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

### I/O specification

As mentioned above, configuring IO is frequently optional - many intermediary processors can infer inputs and outputs correctly, even if they are missing. Configuring IO is needed if you wish to:

1. Have input to a processor come from an earlier processor that doesn't immediately precede it, or from outside the pipeline.
2. Have output from a non-final processor saved, or have multiple output streams saved.

Inputs and outputs are configured similarly, but have slightly different semantics.

Inputs have two fields, only one of which may be specified:
* `name`: A named input. This will use either a named input to the pipeline, or a previous output with the same name.
* `uri`: An input from outside the pipeline, read from the given URI.

Outputs have two fields, where one or both may be specified:
* `name`: A named output. Can be read by later inputs using the same name.
* `uri`: A URI to write output to. Output will be written to this regardless of whether this output is read by a later input.

You can specify inputs and outputs as objects with these keys, but you may also specify them as strings. A string configuration will be treated as a URI if it has a `:` character, and as a name otherwise.

URIs support two schemes:
* `file`: A file on local disk, e.g. `file:///dev/null`. Must be a full path (non-relative). Input will read from this file, and output will overwrite this file with all data produced.
* `aristore`: An aristore document. URIs are of the form `aristore://{documentType}/{datasetName}/{documentId}`. Currently `documentType` must be `file`, for `FileDocument`s. Note that Aristore doesn't allow `/` to appear in dataset names or document IDs, so the URI will be unambiguous. If you wish to output a variable number of files to a single dataset, you can omit the final `/{documentId}`. This will cause the dataset directory to be passed in to the `Processor`, and any files created in that directory will be uploaded to Aristore upon pipeline completion.

For input, Aristore documents are read at pipeline execution time, and will use the latest version of the document available.

For output, Ermine will collect all documents written by dataset, and commit them in a batch after the pipeline completes successfully. New datasets will be created automatically, if needed. `FileDocument`s will use the document ID as the filename.


[The Ferret example](https://github.com/allenai/extraction/blob/master/ermine/examples/ferret.conf) is a good place to look for a sample configuration.
[The Aristore example](https://github.com/allenai/extraction/blob/master/ermine/examples/aristore.conf) has sample Aristore I/O.



## Running ermine

Ermine is run with the sbt `run` command. You can run a pipeline like:

`sbt "ermine/run -c examples/ferret.conf -i some-sentences-file.txt -o output.ttl"`

The `-c` flag is the only required one, and it specifies the configuration file for the pipeline you're running. the `-i` and `-o` flags are only used if your pipeline doesn't specify a first-stage input or a last-stage output (respectively). `-i` can be repeated if you have multiple inputs to the start of your pipeline.
