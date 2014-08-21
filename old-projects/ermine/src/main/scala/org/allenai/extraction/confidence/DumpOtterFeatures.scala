package org.allenai.extraction.confidence

import org.allenai.extraction.processors.definition.OtterExtraction
import org.allenai.extraction.FlatProcessor

import spray.json.pimpAny
import spray.json.pimpString

import scala.io.Source

import java.io.Writer

/** This processor takes as input the JSON output from Otter and produces a file
  * with extractions formatted for tagging and feature values for training
  * confidence scoring functions.
  *
  * In the output file a sample extraction will look like
  *
  * ;;; DEFINITION:   abacus | Noun | An abacus is an ancient calculating device. It is made of a frame with beads on various rods.
  * ;;; [ abacus | isa/ISA | calculating device ]<TAB>0.8535462065874246<TAB>[0|An|an|DT|B-NP] [1|abacus|abacus|NN|I-NP] ...<TAB>0.0<TAB>0.0<TAB>1.0<TAB>...
  *
  * where the tab separated fields are:
  * Pretty-printed extraction<TAB>confidence<TAB>annotated tokens<TAB>tab-separated feature values
  *
  */
object DumpOtterFeatures extends FlatProcessor {

  override def processText(source: Source, sink: Writer): Unit = {
    sink.write("*** Otter Dump for " + source.descr + " ***\n")
    val conf = ExtractionConfidenceFunction.loadDefaultOtterClassifier()
    sink.write("\n*** FEATURES ***\n")
    OtterFeatures.featureMap.keys.foreach { name => sink.write("::: " + name + "\n") }
    sink.write("\n*** EXTRACTIONS ***\n")
    // Iterate over input JSONs and process definitions.
    for (rawLine <- source.getLines) {
      val line = rawLine.trim
      // Skip over first line- it is expected to contain a "[",  the last line which is expected 
      // to contain a "]" and lines in the middle that have just a ",". 
      if (!(line.length == 0) && !line.equals("[") && !line.equals("]") && !line.equals(",")) {
        val otterExtraction = line.parseJson.convertTo[OtterExtraction]
        for {
          extrAlts <- otterExtraction.extractions
          otterTokens = extrAlts.preprocessedDefinitionTokens
        } {
          sink.write(";;; DEFINITION:   " + otterExtraction.definedTerm + " | " +
            otterExtraction.wordClass.getOrElse("NONE") + " | " + extrAlts.preprocessedDefinition + "\n")
          for {
            extr <- extrAlts.extractions
            extrAnno = OtterExtractionTupleAnnotated(extr.tuple, otterTokens)
            score = conf(extr.tuple.relation.relationType.toString)(extrAnno)
            features = OtterFeatures.featureMap.map { case (name, feature) => feature(extrAnno) }
          } {
            sink.write(
              ";;; " + extr.tuple.toSimpleString + "\t" + score + "\t" +
                otterTokens.map(_.toFullString).mkString(" ") +
                "\t" + features.mkString("\t") + "\n")
          }
        }
      }
    }
    sink.write("*** END OF FILE ***")
  }
}
