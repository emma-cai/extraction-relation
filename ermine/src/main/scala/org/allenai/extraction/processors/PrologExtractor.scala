package org.allenai.extraction.processors

import org.allenai.common.Resource
import org.allenai.extraction.{ FlatProcessor, Processor }

import jpl.{ JPL, Term, Query }

import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.Writer

/** Class responsible for holding a reference to the JPL initialized with the Ferret libraries.
  * @param ferretDir the root of the ferret installation
  */
class Ferret(ferretDir: String) {
  /** Initialize JPL. This is lazily evaluated to keep this from breaking class
    * load if we don't have the swipl libraries on the classpath.
    */
  lazy val jpl = {
    classOf[JPL].synchronized {
      // Be quiet (don't print informational load messages).
      JPL.init(JPL.getDefaultInitArgs :+ "-q")
      val loadProlog = new Query(
        s"consult(['${ferretDir}/relation.pl', '${ferretDir}/patterns-stanford.pl'])")
      loadProlog.allSolutions
      loadProlog.rewind
    }
    classOf[JPL]
  }
}

object PrologProcessor {
  /** Name used for the Prolog variable we're targeting. */
  val VariableName = "Var"
}

/** Prolog processor running the Ferret extraction code. This has two concrete instances - one for
  * extractions, and one for question analysis.
  *
  * @param ferret reference to the ferret initializer class
  * @param prologGoal the prolog goal code to use. Should have a variable named
  *   PrologProcessor.VariableName.
  */
class PrologProcessor(val ferret: Ferret, val prologGoal: String) extends FlatProcessor {
  override protected def processInternal(source: Source, destination: Writer): Unit = {
    // First step: Write the TTL input to a file so that prolog can run on it.
    val ttlFile = File.createTempFile("prolog-input-", ".ttl")
    ttlFile.deleteOnExit
    Resource.using(new FileWriter(ttlFile)) { ttlFileWriter =>
      for (line <- source.getLines) {
        ttlFileWriter.write(line)
        ttlFileWriter.write("\n")
      }
    }

    val results = ferret.jpl.synchronized {
      // Next, run the prolog extractor and generate output rules.
      val solveRelations = new Query(
        s"rdf_load('${ttlFile.getAbsolutePath()}'), " +
        s"${prologGoal}")

      val prologResults = for {
        result <- solveRelations.allSolutions
        // TODO(jkinkead): This isn't robust to prolog failures - have a sensible default.
        text = result.get(PrologProcessor.VariableName) match {
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

/** Processor for text. */
class FerretTextProcessor(ferret: Ferret) extends
  PrologProcessor(ferret, s"relation(${PrologProcessor.VariableName}, _)")

/** Processor for questions. Takes one stream for the question and one for the focus. */
class FerretQuestionProcessor(val ferret: Ferret) extends Processor {
  override val numInputs = 2
  override val numOutputs = 1

  override protected def processInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    val question = sources(0)
    val focus = sources(1).getLines.mkString("\n")

    // Internal PrologProcessor we delegate to.
    val internalProcessor =
      new PrologProcessor(ferret, s"question('${focus}', ${PrologProcessor.VariableName})")
    internalProcessor.process(Seq(question), destinations)
  }
}
