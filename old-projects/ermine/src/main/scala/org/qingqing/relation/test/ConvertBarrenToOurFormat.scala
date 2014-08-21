package org.qingqing.relation.test

import scala.io.Source
import scala.collection.mutable.Map
import org.qingqing.relation.util._

object ConvertBarrenToOurFormat {
  def main(args: Array[String]) = {
    val MYWrite = new Write()
    val tuplesList = getDataFromExtraction("/Users/qingqingcai/Documents/Data/Barrons/barrons.txt.rnn.out.txt")
    tuplesList.foreach(println)
    val output = "/Users/qingqingcai/Documents/Data/Barrons/disrel_tuples.barrons.txt"
    MYWrite.rewrite_1(output, tuplesList)
  }

  def getDataFromExtraction(inputname: String): List[(String, String, String, String, String)] = {
    val reg = new RegularExp()
    var tuplesList: List[(String, String, String, String, String)] = List()
    val lines = Source.fromFile(inputname).getLines().toList
    var i = 0
    while (i < lines.size - 1) {
      var line = lines(i)
      var nextline = lines(i + 1)

      if (line.startsWith(";;;") && nextline.startsWith("%")) {
        //extract sentence from line
        var sen = line.substring(3, line.length()).trim()

        //extract relation from nextline
        var arr = nextline.split("\t");
        if (arr.length == 3) {
          val arg1str = arr(0)
          val relstr = arr(1)
          val arg2str = arr(2)
          val arg1set = reg.findAllSubstrings(arg1str, "\"", "\"")
          val arg2set = reg.findAllSubstrings(arg2str, "\"", "\"")
          val relpha_disrel = relstr.split("/")
          val relpha = reg.findAllSubstrings(relpha_disrel(0), "\"", "\"")
          val disrel = relpha_disrel(1)
          val tuple = (disrel, relpha.mkString(" "), arg1set.mkString(" "), arg2set.mkString(" "), sen)
          tuplesList = tuplesList ::: List(tuple)
        }

        i = i + 2
        nextline = lines(i)
        while (!nextline.startsWith(";;;")) {
          //extract relations from nextline, for the same sentence
          var arr = nextline.split("\t");
          if (arr.length == 3) {
            val arg1str = arr(0)
            val relstr = arr(1)
            val arg2str = arr(2)
            val arg1set = reg.findAllSubstrings(arg1str, "\"", "\"")
            val arg2set = reg.findAllSubstrings(arg2str, "\"", "\"")
            val relpha_disrel = relstr.split("/")
            val relpha = reg.findAllSubstrings(relpha_disrel(0), "\"", "\"")
            val disrel = relpha_disrel(1)
            val tuple = (disrel, relpha.mkString(" "), arg1set.mkString(" "), arg2set.mkString(" "), sen)
            tuplesList = tuplesList ::: List(tuple)
          }

          i = i + 1
          nextline = lines(i)
        }
      } else {
        i = i + 1
      }
    }
    return tuplesList
  }

  /** update sen_relsList for specific sen, given a new rel **/
  def update(sen_relsList: Map[String, List[String]], sen: String, rel: String) = {
    if (sen_relsList.contains(sen)) {
      var relsList: List[String] = sen_relsList(sen)
      if (!relsList.contains(rel))
        relsList = relsList ::: List(rel)
      sen_relsList.put(sen, relsList)
    } else {
      sen_relsList.put(sen, List(rel))
    }
  }
}