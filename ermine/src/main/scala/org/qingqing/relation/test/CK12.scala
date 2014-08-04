package org.qingqing.relation.test

import org.qingqing.relation.util
import org.qingqing.relation.util.Indexing
import org.qingqing.relation.util.Searching

object CK12 {
  def main(args: Array[String]) = {
    val source = "/Users/qingqingcai/Documents/Data/CK12/ck12-biology-sentences.txt"
    val ReverbIndexPath = "/Users/qingqingcai/Documents/Data/Reverb/Index"
    val CK12IndexPath = "/Users/qingqingcai/Documents/Data/CK12/Index"
    
//    //build index
//    val ck12Index:Indexing = new Indexing()
//    ck12Index.CK12Index(source, indexPath)
    
    //do search
    val ck12Search:Searching = new Searching()
    val arg1 = "a government"
    val list1 = ck12Search.runSearch(CK12IndexPath,"\"" + arg1 + "\"", "sen", List("sen"), 1000000)
    list1.foreach(println)
    println(list1.size)
    
    
    //do reverb search
//    val arg1 = "the disaster"
//    val arg2 = "GOD"
//    val reverbSearch:Searching = new Searching()
//    val list2 = reverbSearch.runSearch(ReverbIndexPath, "\""+arg1+"\"", "\""+arg2+"\"", "arg1", "arg2", List("kp"), 1000)
//    println(list2.size)
//    list2.foreach(println)
  }
}