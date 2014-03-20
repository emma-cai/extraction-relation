package org.allenai.extraction

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.tokenize.Tokenizer

import spray.json._
import spray.json.DefaultJsonProtocol._

package object interface {
  /** Token pulled from a string.
    * TODO(jkinkead): Document allowed POS and chunk values.
    * @param string the raw token
    * @param lemma the lemmatized token
    * @param posTag part-of-speech tag for the token
    * @param chunk the labeled chunk for the token
    * @param offset the offset into the original string, for reconstructing source text from
    *     sequences of tokens
    */
  case class Token(string: String, lemma: String, posTag: Symbol, chunk: Symbol, offset: Int) {
    def toNlpToken(): Lemmatized[ChunkedToken] = {
      Lemmatized(ChunkedToken(chunk.toString, posTag.toString, string, offset), lemma)
    }
  }
  object Token {
    implicit val tokenJsonFormat = jsonFormat5(Token.apply)

    def fromNlpToken(lemmatized: Lemmatized[ChunkedToken]): Token = lemmatized match {
      case Lemmatized(ChunkedToken(chunk, posTag, string, offset), lemma) =>
        Token(string, lemma, Symbol(posTag), Symbol(chunk), offset)
    }

    /**
      * Rebuild the original text from a series of tokens.  This will fill any missing text with
      * the same quantity of spaces.
      * @param leftJustify if set to true, remove any leading whitespace before the first token
      */
    def originalText(tokens: Seq[Token], leftJustify: Boolean = false) = {
      val nlpTokens = tokens map { _.toNlpToken.token }
      if (leftJustify && tokens.nonEmpty) {
        Tokenizer.originalText(nlpTokens, nlpTokens.head.offset)
      } else {
        Tokenizer.originalText(nlpTokens)
      }
    }
  }

  /** A coreference in a range of tokens. This will contain a label matching at least one other
    * coreference in the parent Rule.
    * @param sourceRange the range of the coreference in the containing phrase's tokens
    * @param label the variable label for this coreference, matching at least one other coference in
    *     the containing Rule
    */
  case class Coreference(sourceRange: Range, label: String)
  object Coreference {
    implicit val rangeJsonFormat = new RootJsonFormat[Range] {
      def write(value: Range): JsObject = {
        JsObject("start" -> JsNumber(value.start), "end" -> JsNumber(value.end))
      }

      def read(value: JsValue): Range = value.asJsObject.getFields("start", "end") match {
        case Seq(JsNumber(start), JsNumber(end)) => (start.toInt until end.toInt)
        case _ => throw new DeserializationException("Range expected")
      }
    }
    implicit val coreferenceJsonFormat = jsonFormat2(Coreference.apply)
  }

  /** The subject or the object of a tuple. May be inferred, such as the object in the fragment
    * "animals eat" => ("animals", "eat", "").
    * @param string the tokenized text from the original source. May be empty in the case where this
    *     is an inferred NounPhrase.
    * @param coreferences a list of all coreferences occuring within this phrase. The sourceRange
    *     values in these coreferences map into the 'string' token list.
    * @param isInferred if true, this is an inferred noun (and by implication, 'string' will be
    *     empty)
    */
  case class NounPhrase(string: Seq[Token], coreferences: Seq[Coreference], isInferred: Boolean)
  object NounPhrase {
    implicit val nounPhraseJsonFormat = jsonFormat3(NounPhrase.apply)
  }

  /** An extracted Tuple.
    * @param subject the subject of the tuple
    * @param verbPhrase the relation for this tuple. Can be empty, e.g. for simple noun phrases.
    * @param directObject the object of the tuple, or None if this doesn't have an object
    * @param extraPhrases any extra context phrases pulled out of the sentence
    */
  case class ExtractionTuple(subject: NounPhrase, verbPhrase: Seq[Token],
    directObject: Option[NounPhrase], extraPhrases: Seq[Seq[Token]])
  object ExtractionTuple {
    implicit val extractionTupleJsonFormat = jsonFormat4(ExtractionTuple.apply)
  }

  /** A relation with some normalization attempted.
    * @param string the string from the original sentence. May be empty if the relation was inferred
    *     from a sentence.
    * @param normalizedRelation the normalized form of the relation. None if the relation couldn't
    *     be normalized.
    */
  case class Relation(string: Seq[Token], normalizedRelation: Option[Relation.Normalized])
  object Relation {
    sealed class Normalized {
      def name = this.getClass.getSimpleName
    }
    object Normalized {
      def fromString(value: String): Normalized = value match {
        case "Cause" => Cause()
        case "Enable" => Enable()
        case "ExampleOf" => ExampleOf()
        case "Purpose" => Purpose()
        case _ => throw new IllegalArgumentException("unknown Relation.Normalized value: " + value)
      }

      /** Format for serializing our enum-like Normalized case classes. */
      implicit val normalizedJsonFormat = new JsonFormat[Normalized] {
        def write(value: Normalized) = JsString(value.name)

        def read(value: JsValue): Normalized = value match {
          case JsString(name) => try {
            fromString(name)
          } catch {
            case e: IllegalArgumentException => throw new DeserializationException(e.getMessage())
          }
          case _ => throw new DeserializationException("JsString expected")
        }
      }
    }
    case class Cause() extends Normalized
    case class Enable() extends Normalized
    case class ExampleOf() extends Normalized
    case class Purpose() extends Normalized

    implicit val relationJsonFormat = jsonFormat2(Relation.apply)
  }

  /** A single extracted relation. This is essentially a recursive Tuple of depth 2. If multiple
    * antecedents or consequents are present, they should be considered logical conjunctions - all
    * antecedents must be true for the relation to exist; and if it does, all consequents have the
    * relation.
    */
  case class ExtractionRule(antecedents: Seq[ExtractionTuple], relation: Relation,
    consequents: Seq[ExtractionTuple], confidence: Double)
  object ExtractionRule {
    implicit val extractionRuleJsonFormat = jsonFormat4(ExtractionRule.apply)
  }
}
