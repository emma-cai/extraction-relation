package org.allenai.extraction.confidence

import org.allenai.extraction.processors.definition.OtterExtraction
import org.allenai.extraction.FlatProcessor

import spray.json.pimpAny
import spray.json.pimpString

import scala.io.Source

import java.io.Writer

/** This processor takes as input the JSON output from Otter and produces a file
  * with extractions formatted for tagging and feature values for training
  * confidence scoring functions
  */
object DumpOtterFeatures extends FlatProcessor {

  override def processText(source: Source, sink: Writer): Unit = {
    sink.write("*** Otter Dump for " + source.descr + " ***\n")
    val conf = ExtractionConfidenceFunction.loadDefaultOtterClassifier()
    sink.write("\n*** FEATURES ***\n")
    for { (name, feature) <- OtterFeatures.featureMap }
      sink.write("::: " + name + "\n")
    sink.write("\n*** EXTRACTIONS ***\n")
    // Iterate over input JSONs and process definitions.
    for (rawLine <- source.getLines) {
      val line = rawLine.trim()
      // Skip over first line- it is expected to contain a "[",  the last line which is expected 
      // to contain a "]" and lines in the middle that have just a ",". 
      if (!(line.length == 0) && !line.equals("[") && !line.equals("]") && !line.equals(",")) {
        val extrs0 = line.parseJson
        val extrs = extrs0.convertTo[OtterExtraction]
        for {
          extrAlts <- extrs.extractions
          otterTokens = extrAlts.preprocessedDefinitionTokens
        } {
          sink.write(";;; DEFINITION:   " + extrs.definedTerm + " | " +
            extrs.wordClass.getOrElse("NONE") + " | " + extrAlts.preprocessedDefinition + "\n")
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
