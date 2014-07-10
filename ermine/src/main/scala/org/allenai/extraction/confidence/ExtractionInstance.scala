package org.allenai.extraction.confidence

import org.allenai.extraction.confidence.ExtractionInstance.TokenMap
import org.allenai.extraction.processors.definition.{
  Argument,
  ComplexOtterExtractionTuple,
  OtterExtractionTuple,
  OtterExtractionTupleWithTupleRelObject,
  OtterToken,
  SimpleOtterExtractionTuple
}

/* ExtractionInstance is intended to be a general representation of an extraction
 * which is geared towards feature extraction for confidence functions */
case class ExtractionInstance(sourceText: String, tokenMap: TokenMap, extraction: ExtractionTuple) {
  def prettyPrint = {
    extraction match {
      case t: ExtractionTuple => t.agent.get.prettyPrint + " --" + t.relation.semanticLabel.toUpperCase + "-> " + t.dObject.get.prettyPrint
    }
  }
  val sentenceNumber = tokenMap.keys.map(_._1).max
  val maxTokenId = tokenMap.keys.filter(_._1 == sentenceNumber).map(_._2).max
  
  def getToken(index:Int): Option[OtterToken] = tokenMap.get((sentenceNumber, index))
}

sealed abstract class ExtractionNodeOrTuple {
  def prettyPrint: String
  def semanticLabel: String
  def isTuple: Boolean
}

case class ExtractionNode(
    nodeId: String, 
    string: String, 
    tokens: Seq[OtterToken], 
    semanticLabel: String = "") extends ExtractionNodeOrTuple {

  val isTuple = false
  
  def prettyPrint: String = {
    val prefix = if (semanticLabel == "") "" else semanticLabel.toUpperCase + ":"
    prefix + "\"" + string + "\""
  }
  
}

case class ExtractionTuple(
    agent: Option[ExtractionNodeOrTuple], 
    relation: ExtractionNodeOrTuple, 
    dObject: Option[ExtractionNodeOrTuple],
    args: Seq[ExtractionNodeOrTuple] = Seq(), 
    semanticLabel: String = "") extends ExtractionNodeOrTuple {
  
  val isTuple = true

  def prettyPrint: String = {
    val emptyArgString = "-"
    val agentString = agent map (_.prettyPrint) getOrElse (emptyArgString)
    val relString = relation.prettyPrint
    val dObjectString = dObject map (_.prettyPrint) getOrElse (emptyArgString)
    val argsString = if (args.isEmpty) "" else (args map (_.prettyPrint)).mkString(" ", " ", "")
    s"[ $agentString $relString $dObjectString$argsString ]"
  }
}

object ExtractionInstance {
  type TokenMap = Map[(Int, Int), OtterToken]
}
