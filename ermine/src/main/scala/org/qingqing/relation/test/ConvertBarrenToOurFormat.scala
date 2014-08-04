//package org.qingqing.relation.test
//
//import scala.io.Source
//import scala.collection.mutable.Map
//
//object ConvertBarrenToOurFormat {
//  def main(args: Array[String]) = {
//    val sen_relsList = getDataFromExtraction("/Users/qingqingcai/Documents/Data/Barrons/barrons.txt.rnn.out.txt")
//    sen_relsList.foreach(println)
//  }
//  
//  def getDataFromExtraction(inputname: String):Map[String, List[String]] = {
//    var sen_relsList = collection.mutable.Map.empty[String, List[String]]
//    val lines = Source.fromFile(inputname).getLines().toList
//    var i = 0
//    while(i < lines.size-1) {
//      var line = lines(i)
//      var nextline = lines(i+1)
//      
//      if(line.startsWith(";;;") && nextline.startsWith("%")) {
//        //extract sentence from line
//        var sen = line.substring(3, line.length()).trim()
//        var relsList:List[String] = List()
//        
//        
//        sen_relsList.put(sen, relsList)
//        //extract relation from nextline
//        var arr = nextline.split("\t");
//        if(arr.length == 3) { 
//          val arg1str = arr(0)
//          val relstr = arr(1)
//          val arg2str = arr(2)
//          
//          var rel = tmp.substring(tmp.indexOf("/")+1, tmp.length()).trim()
//          update(sen_relsList, sen, rel)
//        }
//        
//        i = i+2
//        nextline = lines(i)
//        while(!nextline.startsWith(";;;")) {
//          //extract relations from nextline, for the same sentence
//          if(arr.length == 3) { 
//            var tmp:String = arr(1)
//            var rel = tmp.substring(tmp.indexOf("/")+1, tmp.length()).trim()
//            update(sen_relsList, sen, rel)
//          }
//          
//          i = i+1
//          nextline = lines(i)
//        }
//      }else{
//        i = i+1
//      }
//    }
//    return sen_relsList
//  }
//
//  /** update sen_relsList for specific sen, given a new rel **/
//  def update(sen_relsList:Map[String, List[String]], sen:String, rel:String) = {
//    if(sen_relsList.contains(sen)) {
//      var relsList:List[String] = sen_relsList(sen)
//      if(!relsList.contains(rel))
//        relsList = relsList ::: List(rel)
//      sen_relsList.put(sen, relsList)
//    }else{
//      sen_relsList.put(sen, List(rel))
//    }
//  }
//}