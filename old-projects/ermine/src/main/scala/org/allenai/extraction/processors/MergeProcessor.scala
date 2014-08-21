package org.allenai.extraction.processors

import org.allenai.extraction.{ ErmineException, Processor }

import java.io.{ File, FileWriter }

/** A processor that takes a single input (usually a directory of multiple files) and merges all
  * text from all sources in that input into a single output file.
  */
object MergeProcessor extends Processor {
  override val numInputs = 1
  override val numOutputs = 1

  override protected def processInternal(sources: Seq[Processor.Input],
    destinations: Seq[Processor.Output]): Unit = {

    val sourceInstances = sources(0).getSources()

    val destinationFile = destinations(0).getOutputFile()
    if (destinationFile.isDirectory) {
      throw new ErmineException("MergeProcessor can only accept single file output!")
    }
    val destinationWriter = new FileWriter(destinationFile)

    for {
      source <- sourceInstances
      line <- source.getLines
    } {
      destinationWriter.write(line)
      destinationWriter.write("\n")
    }

    sourceInstances foreach { _.close }
    destinationWriter.flush
    destinationWriter.close
  }
}
