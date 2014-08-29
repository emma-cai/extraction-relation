package org.allenai.relation.learning

import scala.io.Source
import java.util.ArrayList

case class BinarySentenceDisrel(sid: String, sentence: String, disrel: String, relphrase: String, arg1: String, arg2: String, annotationOpt: Option[String]) {
  override def toString() = s"$sid\t$sentence\t$disrel\t$relphrase\t$arg1\t$arg2\t${annotationOpt.getOrElse("?")}"
}

object BinarySentenceDisrel {

  def header = "sid\tsentence\tdisrel\trelphrase\targ1\targ2\tannotationOpt"

  def fromTrainingFile(file: String, headerLinesToDrop: Int) = {
    Source.fromFile(file).getLines().drop(headerLinesToDrop).map {
      line =>
        //Assume line is of the following form:
        //Q ID    T/F question    Focus   URL Sentence    Supporting (0-2)?   Necessary rewrite
        //Example
        //44  Is it true that sleet, rain, snow, and hail are forms of precipitation?     precipitation   http://ww2010.atmos.uiuc.edu/%28Gh%29/guides/mtr/cld/prcp/home.rxml Precipitation occurs in a variety of forms; hail, rain, freezing rain, sleet or snow.   2
        val splits = line.toLowerCase().replaceAll("'", "").split("\t")
      //  println("splits size = " + splits.size)
        val annotationOpt = if (splits.size > 6) Some(splits(6).toString()) else None
        
        BinarySentenceDisrel(splits(0).trim(), splits(1).trim(), splits(2).trim(), splits(3).trim(), splits(4).trim(), splits(5).trim(), annotationOpt)
    }.toList
  }

  /** sentence example:
    *
    */
  def fromSingleInstance(sentence: String, disrel: String, arg1: String, arg2: String, label: String) = {
    BinarySentenceDisrel("", sentence, disrel, "", arg1, arg2, Some(label))
  }

  def fromTrainingSentence(sentence: String) = {
    val splits = sentence.toLowerCase().replaceAll("'", "").split("\t")
    val annotationOpt = if (splits.size > 6) Some(splits(6).toString()) else None
    BinarySentenceDisrel(splits(0).trim(), splits(1).trim(), splits(2).trim(), splits(3).trim(), splits(4).trim(), splits(5).trim(), annotationOpt)
  }
}