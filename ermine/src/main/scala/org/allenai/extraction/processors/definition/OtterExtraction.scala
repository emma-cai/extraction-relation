package org.allenai.extraction.processors.definition

import org.allenai.common.{ Enum, EnumCompanion }

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized

import spray.json.DefaultJsonProtocol._
import spray.json._

/** A Case Class to represent the output extraction from OtterDefinitionExtractor.
  * For a certain raw definition this structure ties together that definition line with the sequence of
  * outputs for each (preprocessed) definition alternative.
  * @param corpusName name/path or any identifier for the input definition corpus if available-
  * taken directly from the incoming JSON of the preprocessor output object.
  * @param rawDefinitionId just a sequential index for a raw definition within the given corpus.
  * @param rawDefinitionLine original (raw) Definition before preprocessing- taken directly from the
  * incoming JSON of the preprocessor output object.
  * @param definedTerm  term.
  * @param wordClass word class for the term, for e.g., Noun, Verb etc. usually available in
  * any standard definition corpus.
  * @param extractions set of extractions associated with the given (raw) definition. This includes
  * extractions for all individual preprocessed definitions obtained by splitting the incoming
  * raw definition in cases where there are paraphrases.
  */
case class OtterExtraction(
  corpusName: Option[String],
  rawDefinitionId: Int,
  rawDefinitionLine: String,
  definedTerm: String,
  wordClass: Option[String],
  extractions: Seq[OtterExtractionForDefinitionAlternate])

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
  * @param preprocessedDefinition cleaned-up simple definition, which is one part of a multi-part definition
  * generated by the definition preprocessor.
  * @param preprocessedDefinitionTokens  tokens corresponding to the input definition.
  * @param extractions set of extractions associated with the given simple processedDefinition.
  */
case class OtterExtractionForDefinitionAlternate(
  preprocessedDefinition: String,
  preprocessedDefinitionTokens: Seq[OtterToken],
  extractions: Seq[OtterExtractionTuple])

/** A Case Class to represent a token - derived from Lemmatized[ChunkedToken].
  * @param id id based on the index of the token in the seq of tokens for original sentence
  * @param string original string
  * @param posTag pos tag for the string
  * @param chunk chunker output for the string
  * @param lemma string Lemma
  */
case class OtterToken(
    id: Int,
    string: String,
    posTag: String,
    chunk: String,
    lemma: String) {
  override def toString() = s"$string-$id"
}

/** A Case Class to represent a Tuple argument.
  * @param string string representing the Argument
  * @param tokens set of tokens corresponding to the Argument
  * @param tokenInteval (possibly non-contiguous) interval of token within the set of all sentence tokens
  */
case class Argument(
  string: String,
  tokens: Seq[OtterToken],
  tokenInterval: Option[Interval])

/** An Enumeration of certain "special" predicates that we identify. This list will be enhanced as Otter
  * has coverage for more of these relations that have special meaning/handling in inference.
  */
sealed abstract class RelationTypeEnum(id: String) extends Enum[RelationTypeEnum](id)
object RelationTypeEnum extends EnumCompanion[RelationTypeEnum] {
  case object Context extends RelationTypeEnum("Context")
  case object DefinedTerm extends RelationTypeEnum("DefinedTerm")
  case object IsA extends RelationTypeEnum("IsA")
  case object Quality extends RelationTypeEnum("Quality")
  case object Property extends RelationTypeEnum("Property")
  case object Describes extends RelationTypeEnum("Describes")
  register(Context, DefinedTerm, IsA, Quality, Property, Describes)
}

/** A Case Class to represent a Relation.
  * Some relations have special significance and are called out as special predicates, per the enumeration
  * defined in RelationType. But other relations don't fit into any of these, in which case relationType
  * here would be None.
  * @param relationType (optional) relation type per above enumeration.
  * @param relationPhrase phrase associated with the relation.
  */
case class Relation(
  relationType: Option[RelationTypeEnum],
  relationPhrase: Argument)

/** Abstract base class to represent Otter Extraction Tuples, which can be simple or complex, for which
  * there are two separate concrete classes defined below.
  */
sealed abstract class OtterExtractionTuple {
  def tupleTokens: Seq[OtterToken]
  def tokenInterval: Interval
  def relation: Relation
}

/** A Case Class to represent a simple extraction tuple.
  * E.g.: In "Tom lifted the book carefully off the table", the fields in OtterExtractionTupleSimple will
  * map to the input sentence text as follows-
  * SimpleOtterExtractionTuple {
  * agent: Tom
  * relation: lifted
  * relObj: the book
  * advps: [carefully]
  * pps: [off the table]
  * }
  * @param tupleTokens sequence of tokens for the entire tuple.
  * @param tokenInterval total interval for all the tokens in tupleTokens
  * @param agent agent of the relation (optional). If present this is expected to be a noun phrase.
  * @param relation the relation itself. Basically a verb phrase but we are giving it more structure
  * to identify special predicates. See the definition of the Relation class for details.
  * @param relObj object of the main verb that represents the (above) relation.
  * which is a tuple itself.
  * @param advps adverbial phrases
  * @param pps prepositional phrases
  */
case class SimpleOtterExtractionTuple(
    override val tupleTokens: Seq[OtterToken],
    override val tokenInterval: Interval,
    val agent: Option[Argument],
    override val relation: Relation,
    val relObj: Option[Argument],
    val advps: Seq[Argument],
    val pps: Seq[Argument]) extends OtterExtractionTuple {
  override def toString() = {
    val agentStr = agent match {
      case Some(y) => y.string
      case _ => ""
    }
    val reStrl = relation.relationPhrase.string
    val relObjStr = relObj match {
      case Some(y) => y.string
      case _ => ""
    }
    val advpsStrBuilder = new StringBuilder()
    for (advp <- advps) {
      if (advp.string.length > 0)
        advpsStrBuilder ++= ", " + advp.string
    }
    val ppsStrBuilder = new StringBuilder()
    for (pp <- pps) {
      if (pp.string.length > 0)
        ppsStrBuilder ++= ", " + pp.string
    }

    "(" + agentStr + ", " + reStrl + ", " + relObjStr + ", " + advpsStrBuilder.toString + ppsStrBuilder.toString + ")"
  }
}

/** A Case Class to represent an extraction tuple that takes a simple tuple as the object of the relation.
  * For e.g.,
  * "me laugh" in "made me laugh", where "made" is the relation
  * "decide ..." in "helps decide ...", where "helps" is the  relation
  * "which player is best"  in "decide which player is best", where
  * "decide" is the relation
  * @param tupleTokens sequence of tokens for the entire tuple.
  * @param tokenInterval total interval for all the tokens in tupleTokens
  * @param agent agent of the relation (optional). If present this is expected to be a noun phrase.
  * @param relation the relation itself. Basically a verb phrase but we are giving it more structure
  * to identify special predicates. See the definition of the Relation class for details.
  * @param relObj object of the main verb that represents the (above) relation. This is a
  * SimpleOtterExtractionTuple.
  * @param advps adverbial phrases
  * @param pps prepositional phrases
  */
case class OtterExtractionTupleWithTupleRelObject(
    override val tupleTokens: Seq[OtterToken],
    override val tokenInterval: Interval,
    val agent: Option[Argument],
    override val relation: Relation,
    val relObj: SimpleOtterExtractionTuple,
    val advps: Seq[Argument],
    val pps: Seq[Argument]) extends OtterExtractionTuple {
  override def toString() = {
    val result = new StringBuilder("(")
    agent match {
      case Some(y) => result ++= y.string
      case _ =>
    }
    result ++= ", " + relation.relationPhrase.string + ", "
    result ++= relObj.toString
    result ++= ")"
    result.toString
  }
}


/** A Case Class to represent a complex extraction tuple that ties two simple tuples together by a
  * higher level relation between them.
  * (format: OFF)
  * For e.g., "Exercising regularly helps you stay fit", the fields in OtterExtractionTupleComplex will
  * map to the input sentence text as follows-
  * ComplexOtterExtractionTuple {
  * antecedent: Exercising regularly
  * relation: helps
  * consequent: you stay fit
  * }
  * (format: ON)
  * @param tupleTokens sequence of tokens for the entire tuple.
  * @param tokenInterval total interval for all the tokens in tupleTokens
  * @param antecedent first (tuple) participant in the relation.
  * @param relation relation itself. Basically a verb phrase but we are giving it more structure
  * to identify special predicates. See the definition of the Relation class for details.
  * @param consequent second (tuple) participant in the relation.
  */
case class ComplexOtterExtractionTuple (
  override val tupleTokens: Seq[OtterToken],
  override val tokenInterval: Interval,
  val antecedent: SimpleOtterExtractionTuple, 
  override val relation: Relation,
  val consequent: SimpleOtterExtractionTuple
) extends OtterExtractionTuple {
    override def toString() = {
    val result = new StringBuilder("(")               
    result ++= antecedent.toString
    result ++= ", " + relation.relationPhrase.string + ", "
    result ++= consequent.toString
    result ++= ")"
    result.toString
  }
}


/** Companion Objects for all the above classes : Support JSON serialization/deserialization.
  */

object OtterToken {
  /** @param id Id based on the index of the token in the seq of tokens for original sentence
   *  @param ipToken Input Lemmatized[ChunkedToken] representation
   */
  def apply(id: Int, ipToken: Lemmatized[ChunkedToken]) = {
    new OtterToken(id, ipToken.token.string, ipToken.token.postag, ipToken.token.chunk, ipToken.lemma)
  }
  
  def makeTokenSeq(defnTokens: Seq[Lemmatized[ChunkedToken]], tokenInterval: Interval) : Seq[OtterToken] = {
    implicit val postfix = scala.language.postfixOps
    ((defnTokens slice(tokenInterval.start, tokenInterval.end)) zipWithIndex) map { x => OtterToken(tokenInterval.start + x._2 , x._1) }
  }
  
  def makeTokenSeq(defnTokens: Seq[Lemmatized[ChunkedToken]]) : Seq[OtterToken] = {
    implicit val postfix = scala.language.postfixOps
    (defnTokens zipWithIndex) map { x => OtterToken(x._2 , x._1) }
  }
  
  import spray.json.DefaultJsonProtocol._
  implicit val tokenJsonFormat = jsonFormat5(OtterToken.apply)
} 

object OtterInterval {
  implicit object IntervalJsonFormat extends RootJsonFormat[Interval] {
    def write(i: Interval) =
      JsArray(JsNumber(i.start), JsNumber(i.end))
    
    def read(value: JsValue) = value match {
      case JsArray(JsNumber(start) :: JsNumber(end) :: Nil) =>
        Interval.open(start.toInt, end.toInt) 
      case _ => deserializationError("Interval expected")
    }
  }
}


object Argument {
  import spray.json.DefaultJsonProtocol._
  import OtterInterval.IntervalJsonFormat
  implicit val argumentJsonFormat = jsonFormat3(Argument.apply)
} 

object Relation {
  import spray.json.DefaultJsonProtocol._
  implicit val relationJsonFormat = jsonFormat2(Relation.apply)
} 

object SimpleOtterExtractionTuple {
  import spray.json.DefaultJsonProtocol._
  import OtterInterval.IntervalJsonFormat
  implicit val simpleOtterExtractionTupleJsonFormat = jsonFormat7(SimpleOtterExtractionTuple.apply)
} 

object OtterExtractionTupleWithTupleRelObject {
  import spray.json.DefaultJsonProtocol._
  import OtterInterval.IntervalJsonFormat
  implicit val otterExtractionTupleWithTupleRelObjectJsonFormat = jsonFormat7(OtterExtractionTupleWithTupleRelObject.apply)
}

object ComplexOtterExtractionTuple {
  import spray.json.DefaultJsonProtocol._
  import OtterInterval.IntervalJsonFormat
  implicit val complexOtterExtractionTupleJsonFormat = jsonFormat5(ComplexOtterExtractionTuple.apply)
} 

object OtterExtractionTuple {
    val typeFieldIdentifier = "type" 
    val simpleTupleIdentifier = "simple"
    val tupleWithTupleRelObjectIdentifier = "tupleRelObj"
    val complexTupleIdentifier = "complex"
      
    implicit object OtterExtractionTupleJsonFormat extends RootJsonFormat[OtterExtractionTuple]{
      import OtterInterval.IntervalJsonFormat
      def write(t: OtterExtractionTuple) = t match{
        case simple:SimpleOtterExtractionTuple => packType(simple.toJson, simpleTupleIdentifier)
        case tupleRelObj:OtterExtractionTupleWithTupleRelObject => packType(tupleRelObj.toJson, tupleWithTupleRelObjectIdentifier)
        case complex:ComplexOtterExtractionTuple => packType(complex.toJson, complexTupleIdentifier)
      }

      def packType(json: JsValue, typStr: String) : JsValue = {
        val fields = json.asJsObject.fields
        JsObject(fields + (typeFieldIdentifier -> JsString(typStr)))
      }
    
      def read(value: JsValue) = value match {
        case obj:JsObject if (obj.fields(typeFieldIdentifier).toString.replaceAll("\"", "") == simpleTupleIdentifier) => 
          {
            JsObject(obj.fields - typeFieldIdentifier).convertTo[SimpleOtterExtractionTuple]
          }
        case obj:JsObject if (obj.fields(typeFieldIdentifier).toString.replaceAll("\"", "") == tupleWithTupleRelObjectIdentifier) => 
          {
            JsObject(obj.fields - typeFieldIdentifier).convertTo[OtterExtractionTupleWithTupleRelObject]
          }
        case obj:JsObject if (obj.fields(typeFieldIdentifier).toString.replaceAll("\"", "") == complexTupleIdentifier) => 
          {
            JsObject(obj.fields - typeFieldIdentifier).convertTo[ComplexOtterExtractionTuple]
          }
     }
   }
} 

object OtterExtractionForDefinitionAlternate {
  implicit val otterExtractionAltJsonFormat = jsonFormat3(OtterExtractionForDefinitionAlternate.apply)
} 

object OtterExtraction {
  implicit val otterExtractionJsonFormat = jsonFormat6(OtterExtraction.apply)
} 
