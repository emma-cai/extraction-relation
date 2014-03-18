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

  /** The subject or the object of a tuple.
    * @param string the tokenized text from the original source. May be empty in the case where this
    *     is an inferred NounPhrase.
    * @param variableLabel a label for this subject or object. Present if it's referred to in
    *     another related NounPhrase, or if this is an inferred NounPhrase.
    * @param pronounResolution what this resolves to, if it's a pronoun
    */
  case class NounPhrase(string: Seq[Token], variableLabel: Option[String],
    pronounResolution: Seq[Token])
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
  case class Relation(string: Seq[Token], normalizedRelation: Option[Symbol])
  object Relation {
    implicit val relationJsonFormat = jsonFormat2(Relation.apply)
  }

  /** A single extracted relation. This is essentially a recursive Tuple of depth 2. */
  case class ExtractionRule(antecedent: ExtractionTuple, relation: Relation,
    consequent: ExtractionTuple, confidence: Double)
  object ExtractionRule {
    implicit val extractionRuleJsonFormat = jsonFormat4(ExtractionRule.apply)
  }
}
