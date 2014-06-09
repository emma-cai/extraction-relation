package org.allenai.extraction.processors

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.collection.immutable.Interval

/** A Case Class to represent the output extraction from OtterDefinitionExtractor.
  * For a certain raw definition this structure ties together that definition line with the sequence of 
  * outputs for each (preprocessed) definition alternative.
  * @param corpusName          The name/path or any identifier for the input definition corpus if available- 
  *                            taken directly from the incoming JSON of the preprocessor output object.
  * @param definition          The original (raw) Definition before preprocessing- taken directly from the 
  *                            incoming JSON of the preprocessor output object.
  * @param extractions         The set of extractions associated with the given (raw) definition. This includes
  *                            extractions for all individual preprocessed definitions obtained by splitting the 
  *                            incoming raw definition in cases where there are paraphrases. 
  */
case class OtterExtraction (
   corpusName : Option[String],
   definition : String,
   extractions : Seq[OtterExtractionAlt]
)


/** A Case Class to represent an extraction alternate for a single part of a multi-part definition. 
  * The preprocessor splits up a raw input definition line into multiple parts if there are 
  * alternative interpretations separated by semicolons or the like. The below structure contains 
  * the output for each individual (preprocessed)  part. For e.g., the input raw definition line
  * from SimpleWiktionary:
  * "Academia 	noun	#{{uncountable}} 'Academia' is a word for the group of people who are a part 
  * of the scientific and cultural community; this group of people have attended a university and/or 
  * do research." gets preprocessed into two definitions that get fed to Otter: 
  * "Academia is a word for the group of people who are a part of the scientific and cultural community" 
  * and 
  * "this group  of people have attended a university and/or do research."
  * Each of these is a processedDefinition and there will be two OtterExtractionAlt's generated, one per above.
  * @param processedDefinition          Cleaned-up simple definition, which is one part of a multi-part definition 
  *                                     generated by the definition preprocessor.
  * @param processedDefinitionTokens    The tokens corresponding to the input definition.
  * @param extractions                  The set of extractions associated with the given simple processedDefinition. 
  */
case class OtterExtractionAlt (
   processedDefinition : String,
   processedDefinitionTokens : Seq[Token],
   extractions : Seq[OtterExtractionTuple]
)


/** Abstract base class to represent Otter Extraction Tuples, which can be simple or complex, for which 
  * there are two separate concrete classes defined below.
  */
sealed abstract class OtterExtractionTuple(tokens : Seq[Token]) 


/** A Case Class to represent a token - derived from Lemmatized[ChunkedToken]. 
 *  @param id        Id based on the index of the token in the seq of tokens for original sentence
 *  @param ipToken   Input Lemmtaized[ChunkedToken] representation
 */ 
case class Token(val id: Int, val ipToken: Lemmatized[ChunkedToken]) {
  /** Original string */
  def string = ipToken.token.string
  /** POS tag for the string */
  def posTag = ipToken.token.postag 
  /** Chunker output for the string  */
  def chunk = ipToken.token.chunk 
  /** String Lemma */
  def lemma = ipToken.lemma
  override def toString() = s"$string-$id"
}


/** A Case Class to represent a Tuple argument. 
 *  @param tokens        Set of tokens corresponding to the Argument
 *  @param tokenInteval  (Possibly noun-contiguous) interval of token within the set of all sentence tokens
 */ 
case class Argument (
  tokens : Seq[Token],
  tokenInterval : Interval
)


/** A Case Class to represent a simple extraction tuple.
  * E.g.: In "Tom lifted the book carefully off the table", the fields in OtterExtractionTupleSimple will
  * map to the input sentence text as follows-
  * OtterExtractionTupleSimple {
  *   agent: Tom
  *   relation: lifted
  *   relObj: the book
  *   advps: [carefully]
  *   pps: [off the table]
  * }
  * @param tupleTokens  The sequence of tokens for the entire tuple.
  * @param agent        The agent of the relation (optional). If present this is expected to be a noun phrase.
  * @param relation     The relation itself. Basically a verb phrase but we are giving it more structure
  *                     to identify special predicates. See the definition of the Relation class for details.
  * @param relObj       The object of the main verb that represents the (above) relation. This can be a
  *                     simple object, which would be either a noun (phrase) or an adjective (phrase), or a
  *                     complex object which is a tuple itself.
  * @param advps        Adverbial phrases
  * @param pps          Prepositional phrases
  */
case class OtterExtractionTupleSimple (
  tupleTokens : Seq[Token],
  agent: Option[Argument], 
  relation: Relation,
  relObj : RelObject,
  advps : Seq[Argument], 
  pps: Seq[Argument] 
) extends OtterExtractionTuple(tupleTokens)

/** A Case Class to represent a complex extraction tuple that ties two simple tuples together by a 
  * higher level relation between them.
  * For e.g., "Exercising regularly helps you stay fit", the fields in OtterExtractionTupleComplex will
  * map to the input sentence text as follows-
  * OtterExtractionTupleComplex {
  *   antecedent: Exercising regularly
  *   relation: helps
  *   consequent: you stay fit
  * }
  * @param tupleTokens  The sequence of tokens for the entire tuple.
  * @param antecedent   The first (tuple) participant in the relation.
  * @param relation     The relation itself. Basically a verb phrase but we are giving it more structure
  *                     to identify special predicates. See the definition of the Relation class for details.
  * @param consequent   The second (tuple) participant in the relation. 
  */
case class OtterExtractionTupleComplex (
  tupleTokens : Seq[Token],
  antecedent: OtterExtractionTupleSimple, 
  relation: Relation,
  consequent: OtterExtractionTupleSimple
) extends OtterExtractionTuple(tupleTokens)


/** An Enumeration of certain "special" predicates that we identify. This list will be enhanced as Otter
  * has coverage for more of these relations that have special meaning/handling in inference. 
  * 
  */
class RelationType
object RelationType extends Enumeration {       
  type RelationType = Value       
  val isa, hasQuality, hasProperty, describes = Value
}

/** A Case Class to represent a Relation.
  * Some relations have special significance and are called out as special predicates, per the enumeration
  * defined in RelationType. But other relations don't fit into any of these, in which case relationType
  * here would be None.
  * @param relationType    (Optional) relation type per above enumeration.
  * @param relationPhrase  The Phrase associated with the relation.
  */
case class Relation (
    relationType: Option[RelationType],
    relationPhrase: Argument
)

/** Abstract base class to represent a Relation Object. This could be simple, like a noun or an adjective,
  * or complex, like a tuple. Each of those are defined by the concrete classes (below).
  */
sealed abstract class RelObject 


/** A Case Class to represent a simple (noun or adjective) object to a Relation.
  * For e.g.:
  * "John" in "gave John", where "gave" is the relation
  * "tall" in "is tall", where "is" is the relation
  * 
  */
case class SimpleRelObject(val obj: Argument) extends RelObject 


/** A Case Class to represent a complex (Tuple) object to a Relation.
  * For e.g.,
  * "me laugh" in "made me laugh", where "made" is the relation 
  * "decide ..." in "helps decide ...", where "helps" is the  relation
  * "which player is best"  in "decide which player is best", where 
  * "decide" is the relation
  */
case class ComplexRelObject
(val obj: OtterExtractionTuple) extends RelObject 


