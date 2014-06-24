package org.allenai.extraction.processors

import org.allenai.extraction.{ ErmineException, Processor }

import scala.io.Source

import java.io.{ File, FileWriter, Writer }

/** Processor for Ari corpus files. These files have two tab-separated values per line: The first is
  * a sentence number, and the second is the sentence. The sentence number consists of dot-separated
  * integers. The first integer represents the ID of the document in the corpus, and the second
  * number represents the top-level section of the document, i.e. chapter.
  *
  * This processor splits the corpus file input into one output file per top-level document section,
  * and outputs only the sentence text. Because of this, the output needs to be a directory.
  */
object CorpusSplitter extends Processor {
  override val numInputs = 1
  override val numOutputs = 1

  val SectionSentence = """^\d+\.(\d+)(?:\.\d+)*\t(.*)""".r

  override protected def processInternal(sources: Seq[Processor.Input],
    destinations: Seq[Processor.Output]): Unit = {

    val text = sources(0).getSources()(0)
    val prefix = if (text.descr != "") {
      s"${text.descr}-"
    } else {
      ""
    }
    val destinationDir = destinations(0).getOutputFile

    require(destinationDir.isDirectory, "Non-directory output given to CorpusSplitter")

    var sectionIndex = 0
    var currSectionId = "-1"
    var currOutput = null: Writer

    for (line <- text.getLines) {
      line match {
        case SectionSentence(sectionString, sentence) => {
          val sectionId = sectionString
          if (sectionId != currSectionId) {
            if (currOutput != null) {
              currOutput.flush()
              currOutput.close()
            }
            sectionIndex += 1
            currOutput = new FileWriter(
              new File(destinationDir, f"${prefix}${sectionIndex}%02d.txt"))
            currSectionId = sectionId
          }
          currOutput.write(sentence)
          currOutput.write('\n')
        }
        case _ => throw new ErmineException(s"Failure splitting line '${line}'")
      }
    }
    text.close()
    if (currOutput != null) {
      currOutput.flush()
      currOutput.close()
    }
  }
}
