package org.allenai.extraction.api.definition

import scala.slick.driver.H2Driver.simple._

/** A minimal Slick-based shim over an h2 definition extraction database on the local filesystem.
  * @param dbPath path of the required database in the file system
  */
class DefinitionExtractionsDB(
    dbPath: String,
    usernameOption: Option[String],
    passwordOption: Option[String]) {

  // H2 embedded db on filesystem. IFEXISTS=TRUE causes h2 to fail if the db files aren't present
  private val DatabaseUrl = s"jdbc:h2:file:${dbPath};IFEXISTS=TRUE";

  private val db = (usernameOption, passwordOption) match {
    case (Some(username), Some(password)) =>
      Database.forURL(DatabaseUrl, driver = "org.h2.Driver", user = username, password = password)
    case (Some(username), None) =>
      Database.forURL(DatabaseUrl, driver = "org.h2.Driver", user = username)
    case _ =>
      Database.forURL(DatabaseUrl, driver = "org.h2.Driver")
  }

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
    db.withSession { implicit session =>
      val query = for {
        de <- definitionExtractions if de.definedTerm.toLowerCase === qterm
      } yield {
        de.* // project query result to tuple
      }

      // execute query and convert into Definition
      query.list map (DefinitionExtraction.tupled)
    }
  }

  /** Add a record to the extractions table.
    */
  def insertDefinitionExtraction(flatDefinitionExtraction: DefinitionExtraction): Unit = {
    db.withSession { implicit session =>
      definitionExtractions.insert(
        (flatDefinitionExtraction.definedTerm.toLowerCase,
          flatDefinitionExtraction.wordClass,
          flatDefinitionExtraction.source,
          flatDefinitionExtraction.alternateDefinition,
          flatDefinitionExtraction.extraction))
    }
  }

  /** Get number of unique terms.
    */
  def getNumberOfDistinctTerms: Int = {
    db.withSession { implicit session =>
      val terms = definitionExtractions map { _.definedTerm }
      // execute query
      val termList: List[String] = terms.list
      termList.toSet[String].size
    }
  }

  /** Get number of unique definitions.
    */
  def getNumberOfDistinctDefinitions: Int = {
    db.withSession { implicit session =>
      val definitions = definitionExtractions map { _.alternateDefinition }
      // execute query
      val definitionList: List[String] = definitions.list
      (definitionList map { x => x.toLowerCase }).toSet[String].size
    }
  }

  /** Get number of unique sources.
    */
  def getNumberOfDistinctSources: Int = {
    db.withSession { implicit session =>
      val sources = definitionExtractions map { _.source }
      // execute query
      val sourceList: List[String] = sources.list
      (sourceList map { x => x.toLowerCase }).toSet[String].size
    }
  }
}

case class DefinitionExtraction(val definedTerm: String, val wordClass: String, source: String, alternateDefinition: String, extraction: String)