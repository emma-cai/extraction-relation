package org.allenai.extraction.extractors

import org.allenai.common.Resource
import org.allenai.extraction.{ Extractor, FlatExtractor }

import jpl.{ JPL, Term, Query }

import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.Writer

object PrologExtractor {
  /** Name used for the Prolog variable we're targeting. */
  val VariableName = "Var"

  /** Initialize JPL. This is lazily evaluated to keep this from breaking class
    * load if we don't have the swipl libraries on the classpath.
    */
  lazy val jplClass = {
    // TODO(jkinkead): Load this from config.
    val prologRoot = "/Users/jkinkead/work/prototyping/prolog/extraction"
    classOf[JPL].synchronized {
      // Be quiet (don't print informational load messages).
      JPL.init(JPL.getDefaultInitArgs :+ "-q")
      val loadProlog = new Query(
        s"consult(['${prologRoot}/relation.pl', '${prologRoot}/patterns-stanford.pl'])")
      loadProlog.allSolutions
      loadProlog.rewind
    }
    classOf[JPL]
  }
}

/** Prolog extractor running the Ferret extraction code. This has two concrete instances - one for
  * extractions, and one for question analysis.
  *
  * @param prologGoal the prolog goal code to use. Should have a variable named
  *   PrologExtractor.VariableName.
  */
class PrologExtractor(val prologGoal: String) extends FlatExtractor {
  override protected def extractInternal(source: Source, destination: Writer): Unit = {
    // First step: Write the TTL input to a file so that prolog can run on it.
    val ttlFile = File.createTempFile("prolog-input-", ".ttl")
    ttlFile.deleteOnExit
    Resource.using(new FileWriter(ttlFile)) { ttlFileWriter =>
      for (line <- source.getLines) {
        ttlFileWriter.write(line)
        ttlFileWriter.write("\n")
      }
    }

    val results = PrologExtractor.jplClass.synchronized {
      // Next, run the prolog extractor and generate output rules.
      val solveRelations = new Query(
        s"rdf_load('${ttlFile.getAbsolutePath()}'), " +
        s"${prologGoal}")

      val prologResults = for {
        result <- solveRelations.allSolutions
        // TODO(jkinkead): This isn't robust to prolog failures - have a sensible default.
        text = result.get(PrologExtractor.VariableName) match {
          case term: Term => term.name
        }
      } yield text

      solveRelations.rewind()

      val rdfUnload = new Query(s"rdf_unload('${ttlFile.getAbsolutePath()}')")
      rdfUnload.allSolutions()
      rdfUnload.rewind()

      // Create immutable copy.
      prologResults.toSeq
    }

    ttlFile.delete()

    destination.write(results.mkString(""))
  }
}

/** Extractor for text. */
object FerretTextExtractor extends PrologExtractor(s"relation(${PrologExtractor.VariableName}, _)")

/** Extractor for questions. Takes one stream for the question and one for the focus. */
object FerretQuestionExtractor extends Extractor {
  override val numInputs = 2
  override val numOutputs = 1

  override protected def extractInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val question = sources(0)
    val focus = sources(1).getLines.mkString("")

    // Internal PrologExtractor we delegate to.
    val internalExtractor =
      new PrologExtractor(s"question('${focus}', ${PrologExtractor.VariableName})")
    internalExtractor.extract(Seq(question), destinations)
  }
}