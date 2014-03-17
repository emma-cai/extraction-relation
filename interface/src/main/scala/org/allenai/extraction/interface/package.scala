package org.allenai.extraction

package object interface {
  /** Token pulled from a string.
    * TODO(jkinkead): Document allowed POS and chunk values.
    * @param string the raw token
    * @param lemma the lemmatized token
    * @param posTag part-of-speech tag for the token
    * @param chunk the labeled chunk for the token
    */
  case class Token(string: String, lemma: String, posTag: Symbol, chunk: Symbol)

  /** The subject or the object of a tuple.
    * @param string the tokenized text from the original source. May be empty in the case where this
    *     is an inferred Noun.
    * @param variableLabel a label for this subject or object. Present if it's referred to in
    *     another related Noun, or if this is an inferred Noun.
    * @param pronounResolution what this resolves to, if it's a pronoun
    */
  case class Noun (string: Seq[Token], variableLabel: Option[String], pronounResolution: Seq[Token])

  /** An extracted Tuple.
    * @param subject the subject of the tuple
    * @param verb the relation for this tuple. Can be empty, e.g. for simple noun phrases.
    * @param directObject the object of the tuple, or None if this doesn't have an object
    * @param extraPhrases any extra context phrases pulled out of the sentence
    */
  case class ExtractionTuple (subject: Noun, verb: Seq[Token], directObject: Option[Noun],
    extraPhrases: Seq[Seq[Token]])

  /** A relation with some normalization attempted.
    * @param string the string from the original sentence. May be empty if the relation was inferred
    *     from a sentence.
    * @param normalizedRelation the normalized form of the relation. None if the relation couldn't
    *     be normalized.
    */
  case class Relation (string: Seq[Token], normalizedRelation: Option[Symbol])

  /** A single extracted relation. This is essentially a recursive Tuple of depth 2. */
  case class ExtractionRule (antecedent: ExtractionTuple, relation: Relation,
    consequent: ExtractionTuple, confidence: Double)
}
