package org.allenai.relation.processors
import org.allenai.relation.util.Searching

object RelationExtractionDemo {

  private val ReverbIndexPath = "/Users/qingqingcai/Documents/Data/Reverb/Index"

  def run(query: String) = {
    val reverbSearch: Searching = new Searching()
    val list2 = reverbSearch.runSearch(ReverbIndexPath, "\"" + query + "\"", "kp", List("kp", "sen"), 1000)
    println(list2.size)
    list2.foreach(println)
  }
}