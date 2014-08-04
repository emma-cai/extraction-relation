package org.qingqing.relation.test

object RegularExpression {
  def main(args:Array[String]) = {
    var line = ":TYPE:MC:1:0:C :TITLE: Which of the following factors is related to the highest rates of mental disorders ?"
//    var reg = "[A-Z]*[^\\.;:]*[^\\.;:?]*"
      var reg = " [A-Z].*"
        println(line)
    if(line != null) {
      var allMatchesTmp = reg.r.findAllIn(line).toList
      var allMatches:List[String] = List()
      allMatches.foreach(println)
    }

  }
}