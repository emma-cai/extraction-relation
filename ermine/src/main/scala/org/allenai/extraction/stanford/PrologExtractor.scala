package org.allenai.extraction.stanford

import org.allenai.extraction.Extractor
import org.allenai.extraction.interface._

import jpl.Term
import jpl.Query

import spray.json._
// Implicits hack: We want to use a custom JSON reader for Seq[Token], so we need to not import the
// seqFormat implicit from DefaultJsonProtocol.
// TODO(jkinkead): Remove the BooleanJsonFormat from here when issue 7 is resolved.
import spray.json.DefaultJsonProtocol.{ seqFormat => _, BooleanJsonFormat => _, _ }

import scala.io.Source

import java.io.File
import java.io.FileOutputStream
import java.io.Writer

object PrologExtractor extends Extractor {
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
    println("wrote ttl output to " + tmpTtlFile)

    // Next, run the prolog extractor and generate output rules.
    apply(tmpTtlFile, tokenMap)
  }

  def apply(ttlFile: File, tokenMap: Map[String, Token]): Seq[ExtractionRule] = {
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

    // Custom implicits for converting from the prolog tokens, which only contain ID strings.
    implicit val seqTokenJsonFormat = new PrologTokenFormat(tokenMap)

    // Normalized format with extra relations.
    // TODO(jkinkead): Remove once https://github.com/allenai/prototyping/issues/6 is resolved.
    implicit val normalizedJsonFormat = new JsonFormat[Relation.Normalized] {
      def fromString(value: String): Relation.Normalized = value match {
        case "CAUSE" => Relation.Cause
        case "ENABLE" => Relation.Enable
        case "EXAMPLE" => Relation.Example
        case "PURPOSE" => Relation.Purpose
        // Extra relations in the prolog solver.
        case "EFFECT" => Relation.Cause
        case "PART" => Relation.Example
        case "WHEN" => Relation.Cause
        case "FUNCTION" => Relation.Purpose
        case "REQUIREMENT" => Relation.Purpose
        case "RELATION" => Relation.Purpose
        case _ => throw new IllegalArgumentException("unknown Relation.Normalized value: " + value)
      }

      override def write(value: Relation.Normalized) = JsString(value.name.toUpperCase)

      override def read(value: JsValue): Relation.Normalized = value match {
        case JsString(name) => try {
          fromString(name)
        } catch {
          case e: IllegalArgumentException => throw new DeserializationException(e.getMessage())
        }
        case _ => throw new DeserializationException("JsString expected for Relation.Normalized")
      }
    }

    // Coreference format that deserializes a raw string.
    // TODO(jkinkead): Remove once https://github.com/allenai/prototyping/issues/8 is resolved.
    implicit val coreferenceJsonFormat = new JsonFormat[Coreference] {
      override def write(value: Coreference) = JsString(value.label)

      override def read(value: JsValue): Coreference = value match {
        case JsString(label) => Coreference(Seq.empty, label)
        case _ => throw new DeserializationException("JsString expected for Coreference")
      }
    }

    // Boolean format that handles raw strings.
    // TODO(jkinkead): Remove once https://github.com/allenai/prototyping/issues/7 is resolved.
    implicit val booleanJsonFormat = new JsonFormat[Boolean] {
      override def write(value: Boolean) = JsString(value.toString)

      override def read(value: JsValue): Boolean = value match {
        case JsString(value) => value match {
          case "false" => false
          case "true" => true
          case _ => throw new DeserializationException("true or false")
        }
        case _ => throw new DeserializationException("JsString expected for Boolean")
      }
    }

    // Manually define the other Seq[T] that we need, since we didn't import the parameterized
    // seqFormat (doing so will cause a compile error).
    implicit val seqCoreferenceJsonFormat = DefaultJsonProtocol.seqFormat[Coreference]
    implicit val seqSeqTokenJsonFormat = DefaultJsonProtocol.seqFormat[Seq[Token]]
    implicit val seqExtractionTupleJsonFormat = DefaultJsonProtocol.seqFormat[ExtractionTuple]

    // Redefine implicits that use (or transitively use) Seq[Token], so that we can use the
    // Seq[Token] we defined.
    implicit val nounPhraseJsonFormat = jsonFormat3(NounPhrase.apply)
    implicit val extractionTupleJsonFormat = jsonFormat4(ExtractionTuple.apply)
    implicit val relationJsonFormat = jsonFormat2(Relation.apply)
    implicit val extractionRuleJsonFormat = jsonFormat4(ExtractionRule.apply)

    (for {
      rawRule <- jsonResults
    } yield try {
      // TODO(jkinkead): Document other missing fields / errors in issues.
      Some(rawRule.convertTo[ExtractionRule])
    } catch {
      case e: DeserializationException => {
        println("Failure parsing " + rawRule)
        println("Message: " + e.getMessage)
        e.printStackTrace(Console.out)
        None
      }
    }).flatten
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
