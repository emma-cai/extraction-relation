package org.allenai.extraction.stanford

import org.allenai.common.Logging
import org.allenai.extraction.Extractor
import org.allenai.extraction.interface._

import jpl.Term
import jpl.Query

import spray.json._
// Implicits hack: We want to use a custom JSON reader for Seq[Token], so we need to not import the
// seqFormat implicit from DefaultJsonProtocol.
import spray.json.DefaultJsonProtocol.{ seqFormat => _, _ }

import scala.io.Source

import java.io.File
import java.io.FileOutputStream
import java.io.Writer

object PrologExtractor extends Extractor with Logging {
  override def extract(source: Source, destination: Writer): Unit = {
    // First step: Convert the Stanford XML of the parse to a TTL file for input to the prolog
    // system.
    val tmpTtlFile = File.createTempFile("output", ".ttl")
    val tmpFileWriter = new FileOutputStream(tmpTtlFile)
    val tokenMap = try {
      StanfordXmlToTtl(source, new FileOutputStream(tmpTtlFile))
    } finally {
      tmpFileWriter.close()
    }
    logger.debug(s"wrote ttl output to ${tmpTtlFile.getAbsolutePath}")

    // Next, run the prolog extractor and generate output rules.
    val extractions = runTtlFile(tmpTtlFile, tokenMap)

    // TODO(jkinkead): Serialize JSON to outfile!
  }

  def runTtlFile(ttlFile: File, tokenMap: Map[String, Token]): Seq[ExtractionRule] = {
    // TODO(jkinkead): Take from config?
    val prefixPath = "/Users/jkinkead/work/prototyping/prolog/extraction"
    val loadProlog = new Query(
      s"consult(['${prefixPath}/relation.pl', '${prefixPath}/patterns-stanford.pl'])," +
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

    // Custom implicit for converting from the prolog output.
    implicit val extractionRuleJsonFormat: JsonFormat[ExtractionRule] = {
      // Special Seq[Token] format that handles ID string -> Token mapping.
      implicit val seqTokenJsonFormat = new PrologTokenFormat(tokenMap)

      // Redefine implicits that use (or transitively use) Seq[Token], so that we can use the
      // Seq[Token] we defined. Include the other Seq[T] that we need, since we didn't import the
      // parameterized seqFormat - doing so will cause a compile error.
      //
      // Note that *order matters* here - we need the implicits from this local file in all cases,
      // which means re-defining implicits in the order that they are needed by the object
      // hierarchy.
      implicit val seqCoreferenceJsonFormat = DefaultJsonProtocol.seqFormat[Coreference]
      implicit val nounPhraseJsonFormat = jsonFormat3(NounPhrase.apply)

      implicit val seqSeqTokenJsonFormat = DefaultJsonProtocol.seqFormat[Seq[Token]]
      implicit val extractionTupleJsonFormat = jsonFormat4(ExtractionTuple.apply)

      implicit val relationJsonFormat = jsonFormat2(Relation.apply)

      implicit val seqExtractionTupleJsonFormat = DefaultJsonProtocol.seqFormat[ExtractionTuple]
      jsonFormat4(ExtractionRule.apply)
    }

    val unflattenedExtractions: Seq[Option[ExtractionRule]] = (for {
      rawRule <- jsonResults
    } yield try {
      rawRule.asJsObject.fields.get("class") match {
        // TODO(jkinkead): Remove when https://github.com/allenai/prototyping/issues/16 is resolved.
        case Some(JsString("ExtractionTuple")) => {
          logger.warn("skpping top-level ExtractionTuple")
          None
        }
        case _ => Some(rawRule.convertTo[ExtractionRule])
      }
    } catch {
      case e: DeserializationException => {
        logger.warn(s"failure parsing ${rawRule}: ${e.getMessage}")
        None
      }
    })
    val flattenedExtractions: Seq[ExtractionRule] = unflattenedExtractions.flatten
    logger.info(s"successfully converted ${flattenedExtractions.size} of " +
      s"${unflattenedExtractions.size} extractions to JSON")
    flattenedExtractions
  }

  /** Class for deserializing the prolog tokens. */
  class PrologTokenFormat(val tokenMap: Map[String, Token]) extends JsonFormat[Seq[Token]] {
    override def write(value: Seq[Token]) = JsArray()

    override def read(value: JsValue): Seq[Token] = value match {
      case JsArray(ids) => (for {
          id <- ids
          idString <- id match {
            case JsString(tokenId) => Some(tokenId)
            // TODO(jkinkead): Warn?
            case _ => None
          }
        } yield tokenMap.get(idString)).flatten
      case _ => throw new DeserializationException("JsArray expected for Seq[Token]")
    }
  }
}
