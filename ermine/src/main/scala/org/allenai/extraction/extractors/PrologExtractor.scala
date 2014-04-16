package org.allenai.extraction.extractors

import org.allenai.common.Resource
import org.allenai.extraction.FlatExtractor

import jpl.Term
import jpl.Query

import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.io.Source

import java.io.File
import java.io.FileWriter
import java.io.Writer

object PrologExtractor extends FlatExtractor {
  // TODO(jkinkead): Take from config.
  val prologRoot = "/Users/jkinkead/work/prototyping/prolog/extraction"

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

    // Next, run the prolog extractor and generate output rules.
    val loadProlog = new Query(
      s"consult(['${prologRoot}/relation.pl', '${prologRoot}/patterns-stanford.pl'])," +
      s"rdf_load('${ttlFile.getAbsolutePath()}'), " +
      // Magic here: Fill in the Json variable with all relations.
      "relation(Json)")

    val jsonResults = (for {
      result <- loadProlog.allSolutions
      // TODO(jkinkead): This isn't robust to prolog failures - have a sensible default.
      jsonString = result.get("Json") match {
        case term: Term => term.name
      }
    } yield JsonParser(jsonString)).toSeq

    // Serialize JSON to outfile.
    destination.write(jsonResults.toJson.prettyPrint)
  }
}
