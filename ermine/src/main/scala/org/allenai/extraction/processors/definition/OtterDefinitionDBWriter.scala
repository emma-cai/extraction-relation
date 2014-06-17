package org.allenai.extraction.processors.definition

import org.allenai.extraction.FlatProcessor

import java.io.Writer

import scala.io.Source
import scala.slick.driver.H2Driver.simple._

import spray.json._

/** A processor that takes Otter Definition Extractor output in JSON format
  * consisting of a sequence of OtterExtraction objects (serialized), and
  * writes each extraction result to an H2 database. Each record written to
  * the database has the following fields:
  * (format: OFF)
  * TERM                 varchar(50)
  * WORDCLASS            varchar(15)
  * SOURCE               varchar(20)
  * ALTERNATEDEFINITION  varchar(2000)
  * EXTRACTION           varchar(3000)
  * (format: ON)
  * @param dbDir the database path
  * @param user username to connect to the database
  * @param password password to connect to the database
  */
class OtterDefinitionDBWriter(dbDir: String, user: String, password: String) extends FlatProcessor {
  val extractionsDb: DefinitionExtractionsDB = new DefinitionExtractionsDB(dbDir, user, password)

  override def processText(jsonInputSource: Source, destination: Writer): Unit = {
    // Iterate over input JSONs and write definition extractions to DB.
    for (rawLine <- jsonInputSource.getLines) {
      val line = rawLine.trim()
      // Skip over first line- it is expected to contain a "[",  the last line which is expected 
      // to contain a "]" and lines in the middle that have just a ",". 
      if (!(line.length == 0) && !line.equals("[") && !line.equals("]") && !line.equals(",")) {
        val otterExtractionAst = line.parseJson
        val otterExtraction = otterExtractionAst.convertTo[OtterExtraction]
        // Iterate over the definition alternates.
        val extractions = for {
          otterExtractionForDefinitionAlt <- otterExtraction.extractions 
          tuple <- otterExtractionForDefinitionAlt.extractions
        } yield {
          val term = otterExtraction.definedTerm
          val wordClass = otterExtraction.wordClass.getOrElse("")
          val source = otterExtraction.corpusName.getOrElse("")
          val altDefinition = otterExtractionForDefinitionAlt.preprocessedDefinition
          // Prefix output extraction with "<RelationType>: "
          val relTypeStr = (tuple.relation.relationType match {
            case Some(x) => x.toString
            case _ => "Fact"
          }) + ": "
          new DefinitionExtraction(term, wordClass, source, altDefinition, relTypeStr + tuple.toString)
        }
        for (extraction <- extractions) {
          extractionsDb.insertDefinitionExtraction(extraction)
        }
      }
    }
  }
}