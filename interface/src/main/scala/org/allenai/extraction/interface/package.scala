package org.allenai.extraction.interface

/** Token pulled from a string.
  * @param string the raw token
  * @param lemma the lemmatized token
  * @param posTag part-of-speech tag for the token
  * @param chunk the labeled chunk for the token
  */
case class Token(string: String, lemma: String, posTag: Symbol, chunk: String) {
  // TODO(jkinkead): Link to full POS & chunk values.
}

/** The subject or the object of a tuple.
  * @param string the tokenized text from the original source. May be empty in the case where this
  *     is an inferred Noun.
  * @param variableLabel a label for this subject or object. Present if it's referred to in another
  *     related Noun, or if this is an inferred Noun.
  * @param pronounResolution what this resolves to, if it's a pronoun
  */
case class Noun (string: Seq[Token], variableLabel: Option[String], pronounResolution: Seq[Token])

/** An extracted Tuple. Represents a relation between 
case class ExtractionTuple (subject: Noun, verb: Seq[Token],
  val directObject: Option[Noun],
  // Participant(s)?
  val pps: Seq[Seq[Token]]
)
// Relation representation.
class Relation (
  // The original string.
  val string: Seq[Token],
  // The normalized form of the relation, if we can normalize.
  val normalizedRelation: Option[String] 
)
// Extracted relation.
class ExtractionRule (
  val antecedent: ExtractionTuple,
  val relation: Relation,
  val consequent: ExtractionTuple,
  val confidence: Double
)
