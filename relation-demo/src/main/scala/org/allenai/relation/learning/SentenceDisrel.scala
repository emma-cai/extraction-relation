package org.allenai.relation.learning

import scala.io.Source

case class SentenceDisrel1(sid: String, sentence: String, relphrase: String, arg1: String, arg2: String, annotationOpt: Option[String]) {
  override def toString() = s"$sid\t$sentence\t$relphrase\t$arg1\t$arg2\t${annotationOpt.getOrElse("?")}"
}

object SentenceDisrel1 {

  def header = "sid\tsentence\trelphrase\targ1\targ2\tannotationOpt"

  def fromTrainingFile(file: String, headerLinesToDrop: Int) = {
    Source.fromFile(file).getLines().drop(headerLinesToDrop).map {
      line =>
        //Assume line is of the following form:
        //Q ID    T/F question    Focus   URL Sentence    Supporting (0-2)?   Necessary rewrite
        //Example
        //44  Is it true that sleet, rain, snow, and hail are forms of precipitation?     precipitation   http://ww2010.atmos.uiuc.edu/%28Gh%29/guides/mtr/cld/prcp/home.rxml Precipitation occurs in a variety of forms; hail, rain, freezing rain, sleet or snow.   2
        val splits = line.split("\t")
        val annotationOpt = if (splits.size > 5) Some(splits(5).toString()) else None
        SentenceDisrel1(splits(0).trim(), splits(1).trim(), splits(2).trim(), splits(3).trim(), splits(4).trim(), annotationOpt)
    }.toList
  }
}