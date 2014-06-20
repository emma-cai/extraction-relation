package org.allenai.extraction.processors.definition

import org.allenai.extraction.FlatProcessor

import java.io.Writer

import scala.io.Source
import scala.slick.driver.H2Driver.simple._

import spray.json._

/** A processor symmetric to OtterDefinitionDBWriter, meant to support the
  * web demo. It takes a search term and looks up the definition extractions
  * table in the H2 database. Returns the stringified result tuples.
  * @param dbDir the database path
  * @param user username to connect to the database
  * @param password password to connect to the database
  */
class OtterDefinitionDBReader(dbDir: String, user: String, password: String) extends FlatProcessor {
  val extractionsDb: DefinitionExtractionsDB = new DefinitionExtractionsDB(dbDir, user, password)

  /** The main processing method that looks up the given term in the definition
    * extractions database.
    * @param input the definition term we're looking for
    * @param destination a writer to which we will write resulting extractions for the
    * given term
    */
  override def processText(input: Source, destination: Writer): Unit = {

    for (rawLine <- input.getLines) {
      val line = rawLine.trim()
      destination.write("SEARCH TERM: " + line + "\n\n")
      val results = extractionsDb.lookup(line)
      if (results.size == 0) {
        destination.write("No entries for this term in our database!")
      } else {
        // The database is completely flat, so sort the returned query
        // results as required. First sort by the definition.
        val resultMap = results groupBy (_.alternateDefinition)
        for ((definition, v1) <- resultMap) {
          if (definition.length > 0) {
            // Write the definition out
            destination.write("DEFINITION: " + definition + "\n")
            // Then group the value (seq of DefinitionExtractions) further by source
            val perDefinitionSourceMap = v1 groupBy (_.source)
            for ((source, v2) <- perDefinitionSourceMap) {
              // Write the source out
              destination.write("SOURCE: " + source + "\n")
              // Then group the value (seq of DefinitionExtractions) further by word class
              val perDefinitionWordClassMap = v2 groupBy (_.wordClass)
              for ((wordClass, extractions) <- perDefinitionWordClassMap) {
                // Write the word class out
                destination.write("WORD CLASS: " + wordClass + "\n")
                destination.write("\n")
                if (extractions.size == 0) {
                  destination.write("No Extractions Found for the given definition!\n")
                } else {
                  destination.write("EXTRACTIONS\n")
                  // Sort the extractions and write them out
                  extractions sortBy (_.extraction) map { e => destination.write(e.extraction + "\n") }
                }
              }
            }
            destination.write("\n")
          }
        }
        destination.write("\n")
      }
    }
  }
}