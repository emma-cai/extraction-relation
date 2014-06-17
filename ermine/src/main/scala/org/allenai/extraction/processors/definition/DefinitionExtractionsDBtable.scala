package org.allenai.extraction.processors.definition

import scala.slick.driver.H2Driver.simple._

/** A minimal Slick-based shim over an h2 definition extraction database on the local filesystem.
  * @param dbPath path of the required database in the file system
  */
class DefinitionExtractionsDB(dbPath: String) {

  // H2 embedded db on filesystem. IFEXISTS=TRUE causes h2 to fail if the db files aren't present
  private val DatabaseUrl = s"jdbc:h2:file:${dbPath};IFEXISTS=TRUE";

  private val db = Database.forURL(DatabaseUrl, driver = "org.h2.Driver", user = "sbhaktha", password = "test")

  // Create a session for this object to use.
  implicit private val session = db.createSession

  private class DefinitionExtractionsDBtable(tag: Tag) extends Table[(String, String, String, String, String)](tag, "EXTRACTIONS") {
    def definedTerm = column[String]("TERM")
    def wordClass = column[String]("WORDCLASS")
    def source = column[String]("SOURCE")
    def alternateDefinition = column[String]("ALTERNATEDEFINITION")
    def extraction = column[String]("EXTRACTION")

    // Define a * (project) function of the tupleized column values (required by slick)
    def * = (definedTerm, wordClass, source, alternateDefinition, extraction)
  }

  // Define the definitions pseudo-collection TableQuery
  private val definitionExtractions = TableQuery[DefinitionExtractionsDBtable]

  /** Lookup a term's definitions by querying the underlying db for all definitions matching that term.
    */
  def lookup(qterm: String): Seq[DefinitionExtraction] = {
    val query = for {
      de <- definitionExtractions if de.definedTerm === qterm
    } yield {
      de.* // project query result to tuple
    }

    // execute query and convert into Definition
    query.list map (DefinitionExtraction.tupled)
  }

  /** Add a record to the extractions table.
    */
  def insertDefinitionExtraction(flatDefinitionExtraction: DefinitionExtraction): Unit = {
    definitionExtractions.insert(
      (flatDefinitionExtraction.definedTerm,
        flatDefinitionExtraction.wordClass,
        flatDefinitionExtraction.source,
        flatDefinitionExtraction.alternateDefinition,
        flatDefinitionExtraction.extraction))
  }

  /** Close the session
    */
  override def finalize: Unit = {
    session.close
  }
}

case class DefinitionExtraction(val definedTerm: String, val wordClass: String, source: String, alternateDefinition: String, extraction: String)