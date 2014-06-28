package org.allenai.extraction.confidence

import org.allenai.extraction.processors.definition.{
  Argument,
  ComplexOtterExtractionTuple,
  OtterExtractionTuple,
  OtterExtractionTupleWithTupleRelObject,
  OtterToken,
  SimpleOtterExtractionTuple
}

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.collection.immutable.Interval

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

import java.util.regex.Pattern

case class ExtractionInstance(sourceText: String, tokenMap: Map[(Int, Int), OtterToken], extraction: ExtractionTuple) {
  def prettyPrint = {
    extraction match {
      case t: ExtractionTuple => t.agent.get.prettyPrint + " --" + t.relation.semanticLabel.toUpperCase + "-> " + t.dObject.get.prettyPrint
    }
  }
}

sealed abstract class ExtractionNodeOrTuple {
  def prettyPrint: String
  def semanticLabel: String
  def isTuple: Boolean
}

case class ExtractionNode(nodeId: String, string: String, tokens: Seq[OtterToken], semanticLabel: String = "") extends ExtractionNodeOrTuple {
  //def lifted: ExtractionNodeOrTuple = ExtractionNodeOrTuple(node = Some(this), semanticLabel = semanticLabel)
  def prettyPrint: String = {
    val prefix = if (semanticLabel == "") "" else semanticLabel.toUpperCase + ":"
    prefix + "\"" + string + "\""
  }
  val isTuple = false
}

case class ExtractionTuple(agent: Option[ExtractionNodeOrTuple], relation: ExtractionNodeOrTuple, dObject: Option[ExtractionNodeOrTuple],
    args: Seq[ExtractionNodeOrTuple] = Seq(), semanticLabel: String = "") extends ExtractionNodeOrTuple {
  //def lifted: ExtractionNodeOrTuple = ExtractionNodeOrTuple(tuple = Some(this), semanticLabel = semanticLabel)
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



object FerretFeatureSet extends FeatureSet[ExtractionInstance, Double](FerretFeatures.featureMap)

object FerretFeatures {
  type FerretFeature = Feature[ExtractionInstance, Double]
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0

  def isProperNoun(postag: String) = postag == "NNP" || postag == "NNPS"
  def isCommonNoun(postag: String) = postag == "NN" || postag == "NNS"
  def isNoun(postag: String) = isProperNoun(postag) || isCommonNoun(postag)
  def isPluralNoun(postag: String) = postag == "NNS" || postag == "NNPS"
  def isPersonalPronoun(postag: String) = postag == "PRP"
  def isPossessivePronoun(postag: String) = postag == "PRP$"
  def isPronoun(postag: String) = isPersonalPronoun(postag) || isPossessivePronoun(postag)
  def isVerb(postag: String) = postag.startsWith("VB")
  def isPreposition(postag: String) = postag == "IN"

  object antecedentIsTuple extends FerretFeature("antecedent is a tuple") {
    override def apply(extr: ExtractionInstance): Double = {
      extr.extraction.agent match {
        case Some(agent) => agent.isTuple
        case _ => false
      }
    }
  }
/*
  object arg1ContainsPronoun extends FerretFeature("arg1 contains a pronoun or EX") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg1").exists(tok => isPronoun(tok.posTag) || tok.posTag == "EX")
    }
  }

  object arg2sContainsPronoun extends FerretFeature("arg2s contains a pronoun or EX") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg2s").exists(tok => isPronoun(tok.posTag) || tok.posTag == "EX")
    }
  }

  val blacklistArg2 = Set("word", "something", "piece", "part", "kind", "number", "way", "type", "form")

  object blacklistedArg2s extends FerretFeature("arg2s consists of blacklisted words") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg2s").forall(tok => blacklistArg2 contains tok.string)
    }
  }

  object arg1Noun extends FerretFeature("arg1 contains noun") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg1").exists(tok => isNoun(tok.posTag) || isPronoun(tok.posTag))
    }
  }

  object arg2sNoun extends FerretFeature("arg2s contains noun") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg2s").exists(tok => isNoun(tok.posTag) || isPronoun(tok.posTag))
    }
  }

  object arg1Proper extends FerretFeature("arg1 contains proper noun") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg1").exists(tok => isProperNoun(tok.posTag))
    }
  }

  object arg2sProper extends FerretFeature("arg2s contains proper noun") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg2s").exists(tok => isProperNoun(tok.posTag))
    }
  }

  object arg1BeforeRel extends FerretFeature("arg1 appears before rel in sentence") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenSpanInPart(extr, "arg1") leftOf getTokenSpanInPart(extr, "rel")
    }
  }

  object arg2sAfterRel extends FerretFeature("arg2s appears after rel in sentence") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenSpanInPart(extr, "arg2s") rightOf getTokenSpanInPart(extr, "rel")
    }
  }

  object arg1BordersRel extends FerretFeature("arg1 borders rel") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenSpanInPart(extr, "arg1") borders getTokenSpanInPart(extr, "rel")
    }
  }

  object arg2sBordersRel extends FerretFeature("arg2s borders rel") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenSpanInPart(extr, "arg2s") borders getTokenSpanInPart(extr, "rel")
    }
  }

  object relContainsVerb extends FerretFeature("rel contains verb") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "rel") exists (tok => isVerb(tok.posTag))
    }
  }

  object relIsSingleVerb extends FerretFeature("rel is single verb") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      val tokens = getTokensInPart(extr, "rel")
      (tokens.length == 1) && isVerb(tokens.head.posTag)
    }
  }

  object relStartWithVerbEndsWithPrep extends FerretFeature("rel matches VW+P") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      val tokens = getTokensInPart(extr, "rel")
      ((tokens.length > 2) &&
        isVerb(tokens.head.posTag) &&
        isPreposition(tokens.last.posTag))
    }
  }

  object relContiguous extends FerretFeature("rel contiguous") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenSpanInPart(extr, "rel").length == getTokensInPart(extr, "rel").size
    }
  }

  object longArg1 extends FerretFeature("arg1 contains >= 10 tokens") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg1").length >= 10
    }
  }

  object shortArg1 extends FerretFeature("arg1 contains < 3 tokens") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg1").length < 3
    }
  }

  object longArg2s extends FerretFeature("arg2s contains >= 10 tokens") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg2s").length >= 10
    }
  }

  object shortSentence extends FerretFeature("sentence contains < 10 tokens") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      extr.allTokens.length < 10
    }
  }

  object longSentence extends FerretFeature("sentence contains >= 20 tokens") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      extr.allTokens.length >= 20
    }
  }

  object conjBeforeRel extends FerretFeature("conj right before rel") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenRightBefore(extr, "rel") match {
        case Some(t) => t.posTag == "CC"
        case None => 0.0
      }
    }
  }

  object arg1EqualsArg2s extends FerretFeature("arg1 equals arg2s") {
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokensInPart(extr, "arg1").map(_.string).mkString(" ").toLowerCase ==
        getTokensInPart(extr, "arg2s").map(_.string).mkString(" ").toLowerCase
    }
  }

  def getTokenRightBefore(extr: OtterExtractionTupleAnnotated, part: String): Option[OtterToken] = {
    val min = getTokenSpanInPart(extr, part).min
    if (min < 1 || min > extr.allTokens.length) None else Some(extr.allTokens(min - 1))
  }

  def getAllTokensBefore(extr: OtterExtractionTupleAnnotated, part: String): Seq[OtterToken] = {
    val min = getTokenSpanInPart(extr, part).min
    if (min < 0 || min > extr.allTokens.length) Seq() else extr.allTokens.take(min)
  }

  def getTokenRightAfter(extr: OtterExtractionTupleAnnotated, part: String): Option[OtterToken] = {
    val max = getTokenSpanInPart(extr, part).max
    if ((max >= extr.allTokens.length - 1) || (max < 0)) None else Some(extr.allTokens(max + 1))
  }

  def getAllTokensAfter(extr: OtterExtractionTupleAnnotated, part: String): Seq[OtterToken] = {
    val max = getTokenSpanInPart(extr, part).max
    if ((max >= extr.allTokens.length - 1) || (max < 0)) Seq() else extr.allTokens.drop(max + 1)
  }

  def getTokenSpanInPart(extr: OtterExtractionTupleAnnotated, part: String): Interval = {
    val tokens = getTokensInPart(extr, part)
    if (tokens.isEmpty) Interval.empty
    else {
      val ids = tokens.map(_.id)
      Interval.open(ids.min, ids.max + 1)
    }
  }

  def optionalArgumentTokens(arg: Option[Argument]): Seq[OtterToken] = arg match {
    case Some(arg) => arg.tokens
    case _ => Seq()
  }

  def getTokensInPart(extr: OtterExtractionTupleAnnotated, part: String): Seq[OtterToken] = {
    extr.tuple match {
      case extr1: SimpleOtterExtractionTuple => part match {
        case "arg1" => optionalArgumentTokens(extr1.agent)
        case "rel" => extr1.relation.relationPhrase.tokens
        case "arg2s" => optionalArgumentTokens(extr1.relObj) ++ extr1.advps.flatMap(_.tokens) ++ extr1.pps.flatMap(_.tokens)
        case _ => Seq()
      }
      case extr1: OtterExtractionTupleWithTupleRelObject => part match {
        case "arg1" => optionalArgumentTokens(extr1.agent)
        case "rel" => extr1.relation.relationPhrase.tokens
        case "arg2s" => getTokensInPart(OtterExtractionTupleAnnotated(extr1.relObj, extr.allTokens), part) ++ extr1.advps.flatMap(_.tokens) ++ extr1.pps.flatMap(_.tokens)
        case _ => Seq()
      }
      case extr1: ComplexOtterExtractionTuple => getTokensInPart(OtterExtractionTupleAnnotated(extr1.antecedent, extr.allTokens), part) ++
        { if (part == "rel") extr1.relation.relationPhrase.tokens else Seq() } ++
        getTokensInPart(OtterExtractionTupleAnnotated(extr1.consequent, extr.allTokens), part)
    }
  }

  class MatchWordRightBefore(part: String, wordPattern: String,
      labelOverride: String) extends FerretFeature(labelOverride + " right before " + part) {
    def this(part: String, wordPattern: String) = this(part, wordPattern, wordPattern)
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenRightBefore(extr, part) match {
        case Some(t) => t.string.toLowerCase.matches(wordPattern)
        case None => 0.0
      }
    }
  }

  class MatchWordRightAfter(part: String, wordPattern: String,
      labelOverride: String) extends FerretFeature(labelOverride + " right after " + part) {
    def this(part: String, wordPattern: String) = this(part, wordPattern, wordPattern)
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getTokenRightAfter(extr, part) match {
        case Some(t) => t.string.toLowerCase.matches(wordPattern)
        case None => 0.0
      }
    }
  }

  class MatchWordAnywhereBefore(part: String, wordPattern: String,
      labelOverride: String) extends FerretFeature(labelOverride + " anywhere before " + part) {
    def this(part: String, wordPattern: String) = this(part, wordPattern, wordPattern)
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getAllTokensBefore(extr, part) exists (_.string.toLowerCase.matches(wordPattern))
    }
  }

  class MatchWordAnywherAfter(part: String, wordPattern: String,
      labelOverride: String) extends FerretFeature(labelOverride + " anywhere after " + part) {
    def this(part: String, wordPattern: String) = this(part, wordPattern, wordPattern)
    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      getAllTokensAfter(extr, part) exists (_.string.toLowerCase.matches(wordPattern))
    }
  }

  class PartContainsPostag(part: String, postagPattern: String,
      labelOverride: String) extends FerretFeature(s"$part contains $labelOverride") {

    def this(part: String, postagPattern: String) = this(part, postagPattern, postagPattern)

    override def apply(extr: OtterExtractionTupleAnnotated): Double = {
      val tokens = getTokensInPart(extr, part)
      tokens.exists(_.posTag.matches(postagPattern))
    }
  }
*/
  /* the following two lists are copied from ReVerb */
  val comWords = List("acknowledge",
    "add", "address", "admit", "advertise", "advise", "agree",
    "allege", "announce", "answer", "appear", "argue", "ask", "assert",
    "assume", "assure", "believe", "boast", "claim", "comment",
    "complain", "conclude", "confirm", "consider", "contend",
    "convince", "decide", "declare", "demand", "demonstrate", "deny",
    "describe", "determine", "disclose", "discover", "discuss",
    "doubt", "emphasize", "expect", "explain", "express", "fear",
    "feel", "figure", "forget", "hear", "hope", "imply", "indicate",
    "inform", "insist", "instruct", "know", "learn", "maintain",
    "mean", "mention", "note", "notice", "observe", "pray", "predict",
    "proclaim", "promise", "propose", "repeat", "reply", "report",
    "request", "respond", "reveal", "say", "signal", "specify",
    "speculate", "state", "suggest", "teach", "tell", "testify",
    "warn", "write")

  val cogWords = List("estimate",
    "pretend", "prove", "realise", "realize", "recognize", "remember",
    "remind", "saw", "seem", "surmise", "suspect", "suspect",
    "theorize", "think", "understand", "verify", "wish", "worry")

  def features: Seq[FerretFeature] = Seq(
    antecedentIsTuple/*,
    arg1ContainsPronoun,
    arg2sContainsPronoun,
    blacklistedArg2s,
    arg1Noun,
    arg2sNoun,
    arg1Proper,
    arg2sProper,
    arg1BeforeRel,
    arg2sAfterRel,
    arg1BordersRel,
    arg2sBordersRel,
    relContainsVerb,
    relIsSingleVerb,
    relContiguous,
    longArg1,
    longArg2s,
    shortArg1,
    shortSentence,
    longSentence,
    relStartWithVerbEndsWithPrep,
    arg1EqualsArg2s,
    new MatchWordRightBefore("arg1", "if"),
    new MatchWordRightBefore("arg1", "in"),
    new MatchWordRightBefore("arg1", "that|which|who"),
    new MatchWordRightBefore("arg1", ","),
    new MatchWordRightBefore("arg2s", "or"),
    new MatchWordRightBefore("rel", "that"),
    new MatchWordRightBefore("rel", "who|which"),
    new MatchWordRightAfter("arg2s", "'s"),
    new MatchWordRightAfter("arg2s", "of"),
    new MatchWordAnywhereBefore("arg1", "if|whether|though|although"),
    new MatchWordAnywhereBefore("arg1", "may|might|would|could|should|suppose"),
    new MatchWordAnywhereBefore("arg1", comWords.mkString("|"), "communic words"),
    new MatchWordAnywhereBefore("arg1", cogWords.mkString("|"), "cognitive words"),
    new PartContainsPostag("rel", "VBZ"),
    new PartContainsPostag("rel", "VBG"),
    new PartContainsPostag("rel", "VBD"),
    new PartContainsPostag("rel", "VBN"),
    new PartContainsPostag("rel", "VBP"),
    new PartContainsPostag("rel", "VB")*/
    )

  def featureMap: SortedMap[String, FerretFeature] = {
    (for (f <- features) yield (f.name -> Feature.from(f.name, f.apply _)))(scala.collection.breakOut)
  }
}


