package org.qingqing.relation.test

import org.qingqing.relation.util.RegularExp

object RegularExpression {
  def main(args: Array[String]) = {
    val reg = new RegularExp()
    val begLabel = "\""
    val endLabel = "\""
    var line = "\"test\" \" hope\"\"fine\""
    val substrings = reg.findAllSubstrings(line, begLabel, endLabel)
    substrings.foreach(println)
  }
}