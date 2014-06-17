package org.allenai.extraction.processors.definition

import org.allenai.extraction.FlatProcessor

import java.io.Writer

import scala.io.Source

import spray.json.pimpString

/** A simple processor that takes Otter Definition Extractor output in JSON format 
  * consisting of a sequence of OtterExtraction objects (serialized), and
  * creates a human-readable text file with output extractions that has the format of
  * the following sample:
  * (format: OFF)
  * DEFINITION: Chlorophyll	noun	Chlorophyll is a green pigment found in \
  *                                 almost all plants, algae, and cyanobacteria.
  * Defined Term: Chlorophyll
  * Isa: (Chlorophyll, is, pigment)
  * Isa: (Chlorophyll, is, green pigment)
  * Quality: (Chlorophyll, is, green)
  * Fact: (Chlorophyll, found in, almost all plants, algae, and cyanobacteria, , )
  * (format: ON)
  */
object OtterJsonToReadableOutputProcessor extends FlatProcessor {
  
  override def processText(jsonInputSource: Source, destination: Writer): Unit = {
    // Iterate over input JSONs and process definitions.
    for (rawLine <- jsonInputSource.getLines) {
      val line = rawLine.trim()
      // Skip over first line- it is expected to contain a "[",  the last line which is expected 
      // to contain a "]" and lines in the middle that have just a ",". 
      if (!(line.length == 0) && !line.equals("[") && !line.equals("]") && !line.equals(",")) {
        val otterExtractionAst = line.parseJson
        val otterExtraction = otterExtractionAst.convertTo[OtterExtraction]
        // Iterate over the definition alternates and process, i.e., write out each one.
        for (otterExtractionForDefinitionAlt <- otterExtraction.extractions) {
          // First write out the input line.
          destination.write("DEFINITION:   " + otterExtraction.definedTerm + "\t" + otterExtraction.wordClass.getOrElse ("") 
            + "\t" + otterExtractionForDefinitionAlt.preprocessedDefinition + "\n")
          // Then write out the extraction results.
          for (tuple <- otterExtractionForDefinitionAlt.extractions) {
            // Prefix output extraction with "<RelationType>: "
            val relTypeStr =  (tuple.relation.relationType match {
              case Some(x) => x.toString
              case _ => "Fact"
            }) + ": "
            destination.write(relTypeStr)
            // Now write out the actual tuple in friendly format (all tuple classes have overridden toString)
            destination.write(tuple.toString)
            destination.write("\n")
          }
          destination.write("\n")
        }
      }
    }
  }
}
