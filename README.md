# Extraction

This project is meant to contain glue code that integrates our various extraction processes into a single pipeline.

## Project layout

There are three subprojects:

1. interface: A definition of models used as output for extractions (extraction targets)
2. demo: A webapp running an extraction demo
3. ermine: An extraction manager to handle workflows

"ermine" is under construction.


## Running ermine

The main ermine project can be built without any special configuration. However, the `PrologExtractor` requires some special configuration - see below.

After installing prolog and updating `PrologExtractor`, you can run `sbt 'project ermine' stage` to build ermine.

The auto-generated script `ermine/target/universal/stage/bin/extraction-manager` can then be used to run an extraction pipeline:

`./ermine/target/universal/stage/bin/extraction-manager -c ermine/examples/ferret.conf -i some-sentences-file.txt -o output.json`

The `-c` flag is the only required one, and it specifies the configuration file for the pipeline you're running. the `-i` and `-o` flags are only used if your pipeline doesn't specify a first-stage input or a last-stage output (respectively). `-i` can be repeated if you have multiple inputs to the start of your pipeline.


### Installing Prolog

Ermine currently depends on having swipl Prolog installed with the JPL library.

You can install this easily on OS X with [homebrew](http://brew.sh/):

`brew install swi-prolog --with-jpl`

Don't forget the `--with-jpl` flag! The root project's `Build.scala` will check for a valid swipl install at load time, and will print out a warning if one is missing.


## Configuring ermine

Each ermine execution consist of a *pipeline*, which is an ordered list of *processors*. A pipeline simply specifies the order to execute the processors, and what their inputs and outputs are. Each processor can read one or more inputs, and write one or more outputs, determined by their implementation. What ermine does is connect the inputs to the outputs, and save them to a configured location.

Currently, only a small number of processors are supported, and only file (and streaming) output is supported. Integrating the ari data store and improving the processor support is on the TODO list.

Example configurations can be seen in the `ermine/examples` directory.
