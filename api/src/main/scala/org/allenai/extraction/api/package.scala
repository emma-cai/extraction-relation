package org.allenai.extraction

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.tokenize.Tokenizer

import spray.json._
import spray.json.DefaultJsonProtocol._

package object api {
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
    def originalText(tokens: Seq[Token], leftJustify: Boolean = true) = {
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
    * @param sourceTokens the tokens for this coreference
    * @param label the variable label for this coreference, matching at least one other coference in
    *     the containing Rule
    */
  case class Coreference(sourceTokens: Seq[Token], label: String)
  object Coreference {
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
    sealed class Normalized(val name: String)
    object Normalized {
      def fromString(value: String): Normalized = value match {
        case "CAUSE" => Cause
        case "ENABLE" => Enable
        case "EXAMPLE" => Example
        case "PURPOSE" => Purpose
        case "REQUIREMENT" => Requirement
        case _ => throw new IllegalArgumentException("unknown Relation.Normalized value: " + value)
      }

      /** Format for serializing our enum-like Normalized case classes. */
      implicit val normalizedJsonFormat = new JsonFormat[Normalized] {
        def write(value: Normalized) = JsString(value.name.toUpperCase)

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
    case object Cause extends Normalized("CAUSE")
    case object Enable extends Normalized("ENABLE")
    case object Example extends Normalized("EXAMPLE")
    case object Purpose extends Normalized("PURPOSE")
    case object Requirement extends Normalized("REQUIREMENT")

    implicit val relationJsonFormat = jsonFormat2(Relation.apply)
  }

  /** A single extracted relation. This is essentially a recursive Tuple of depth 2. If multiple
    * antecedents or consequents are present, they should be considered logical conjunctions - all
    * antecedents must be true for the relation to exist; and if it does, all consequents have the
    * relation. If relation is missing, only consequants will be considered,
    * and they will be assumed to be simple truthful assertions.
    */
  case class ExtractionRule(antecedents: Seq[ExtractionTuple], relation: Option[Relation],
    consequents: Seq[ExtractionTuple], confidence: Double)
  object ExtractionRule {
    implicit val extractionRuleJsonFormat = jsonFormat4(ExtractionRule.apply)
  }
}
