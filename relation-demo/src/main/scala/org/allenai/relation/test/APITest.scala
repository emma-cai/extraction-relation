package org.allenai.relation.test
import org.allenai.relation.api._
import org.allenai.relation.util._

object APITest {
  def main(args: Array[String]) = {
//    val MYAPI = new SearchInstance()
//    println(MYAPI.insSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", "CAUSE", "because of"))
    
//    val MYSearch: Searching = new Searching()
//    val seed = "because"
//    val perseed = MYSearch.runSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", "\"" + seed + "\"", "kp", List("arg1", "arg2"), 1000)
//    perseed.foreach(println)
//    println(perseed.size)
    
//    val MYAPI = new SentenceSearching()
//    println(MYAPI.senSearch("/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_v2", "CAUSE", "gas", "bacteria"))
    
    val MYAPI = new DependencySearching()
    val res = MYAPI.runSearch("/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_dp", "CAUSE")
    res.foreach(println)
  }
}