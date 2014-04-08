package org.allenai.extraction.extractors

import org.allenai.common.Logging
import org.allenai.extraction.Extractor
import org.allenai.extraction.interface._

import spray.json._
// Implicits hack: We want to use a custom JSON reader for Seq[Token], so we need to not import the
// seqFormat implicit from DefaultJsonProtocol.
import spray.json.DefaultJsonProtocol.{ seqFormat => _, _ }

import scala.io.Source

import java.io.File
import java.io.FileOutputStream
import java.io.Writer

object FerretToExtractionRule extends Extractor with Logging {
  override val numInputs = 2
  override val numOutputs = 1

  override protected def extractInternal(sources: Seq[Source], destinations: Seq[Writer]): Unit = {
    // Local seq format for de/serializing in this function.
    import spray.json.DefaultJsonProtocol.seqFormat

    val ferretSource = sources(0)
    val ferretResults: Seq[JsValue] = {
      JsonParser(ferretSource.getLines.mkString("")).convertTo[Seq[JsValue]]
    }
    val tokensSource = sources(1)
    val tokenMap = JsonParser(tokensSource.getLines.mkString("")).convertTo[Map[String, Token]]

    // Convert to our extraction rule type.
    val extractions = convertFerret(ferretResults, tokenMap)

    // TODO(jkinkead): Output resulting JSON.
    destinations.head.write(extractions.toJson.prettyPrint)
  }

  def convertFerret(ferretResults: Seq[JsValue], tokenMap: Map[String, Token]):
      Seq[ExtractionRule] = {

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
      implicit val coreferenceJsonFormat = jsonFormat2(Coreference.apply)

      implicit val seqCoreferenceJsonFormat = DefaultJsonProtocol.seqFormat[Coreference]
      implicit val nounPhraseJsonFormat = jsonFormat3(NounPhrase.apply)

      implicit val seqSeqTokenJsonFormat = DefaultJsonProtocol.seqFormat[Seq[Token]]
      implicit val extractionTupleJsonFormat = jsonFormat4(ExtractionTuple.apply)

      implicit val relationJsonFormat = jsonFormat2(Relation.apply)

      implicit val seqExtractionTupleJsonFormat = DefaultJsonProtocol.seqFormat[ExtractionTuple]
      jsonFormat4(ExtractionRule.apply)
    }

    val unflattenedExtractions: Seq[Option[ExtractionRule]] = (for {
      rawRule <- ferretResults
    } yield try {
      Some(rawRule.convertTo[ExtractionRule])
    } catch {
      case e: DeserializationException => {
        logger.warn(s"failure parsing ${rawRule.prettyPrint}: ${e.getMessage}")
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
      case JsArray(tokens) => (for {
          token <- tokens
          // TODO(jkinkead): Don't throw away 'text' -
          // https://github.com/allenai/extraction/issues/24
          idString <- token.asJsObject.getFields("token", "text") match {
            case Seq(JsString(tokenId), JsString(text)) => Some(tokenId)
            case _ => throw new DeserializationException(s"malformed token: ${token}")
          }
        } yield tokenMap.get(idString)).flatten
      case _ => throw new DeserializationException("JsArray expected for Seq[Token]")
    }
  }
}
