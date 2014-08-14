package org.allenai.relation.processors
import org.allenai.relation.util.Searching
import org.allenai.relation.util.Indexing
import org.allenai.relation.util.Write

import scala.collection.mutable.Map
object InstanceSelection {
  private val disrel_seeds = collection.immutable.HashMap(
    ("PURPOSE", List("purpose", "used to", "responsible")),
    ("CAUSE", List("caused", "so that", "because", "result in", "effect on")),
    /**("FUNCTION", List("used to")),**/
    ("EXAMPLE", List("an example of", "called", "a way to", "include", "such as")),
    ("ENABLE", List("to help", "by")), 
    ("PART", List("part of")), 
    ("REQUIREMENT", List("necessary", "needed")), 
    ("CONDITION", List("when", "if")))
  
    
  private val stopwordsList = List("is", "am", "are", "was", "were", "be", "has", "have", "had", "the", 
      "a", "an", "having", "being", "do", "did", "done", "doing", "does", "your", "you", "me", "my", "mine", "me", 
      "he", "his", "him", "she", "her", "they", "their", "them", "one", "two", "three", "all", "every", "each", 
      "go", "going", "went", "gone", "some", "any")
      
  private val maxinsres = 10000
  private val CK12IndexPath = "/Users/qingqingcai/Documents/Data/CK12/Index"
  private val output = "/Users/qingqingcai/Documents/Aristo/extraction-new/data/entity_counts_ck12/entity_countsInCK12.txt"
  private var Entity_NumInCK12 = collection.mutable.Map.empty[String, Int]
  def main(args: Array[String]) = {
    insSearch("/Users/qingqingcai/Documents/Data/Reverb/Index")
    println(Entity_NumInCK12.size)
//    Entity_NumInCK12.foreach(println)
    val MYWrite = new Write()
    MYWrite.rewrite_3(output, Entity_NumInCK12)
  }
  
  /** The discouse-relation lexical cue seeds are defined here;
    * for each discourse relation, find all instances which are connected by the lexical-cue-seed
    */
  def insSearch(indexPath: String) = {
    val MYSearch: Searching = new Searching()
    //search for instances given a disrel
    var disrel_insList: Map[String, List[List[String]]] = collection.mutable.Map.empty[String, List[List[String]]]
    disrel_seeds.foreach {
      case (disrel, seeds) => {
        println(disrel)
        var insList: List[List[String]] = List()
        seeds.foreach {
          case seed => { //eg: query = "function of"; output = ("water", "body")
            val perres = MYSearch.runSearch(indexPath, "\"" + seed + "\"", "kp", List("arg1", "arg2"), maxinsres)
            perres.foreach {
              case p => {
                val arg1 = p(0)
                val arg2 = p(1)
                var arg1SizeInCK12 = 0
                var arg2SizeInCK12 = 0
                //check arg1
                if(!Entity_NumInCK12.contains(arg1)) {
                  arg1SizeInCK12 = MYSearch.runSearch(CK12IndexPath, "\""+arg1+"\"", "sen", List("sen"), 1000000).size
                  Entity_NumInCK12.put(arg1, arg1SizeInCK12)
                }else {
                  arg1SizeInCK12 = Entity_NumInCK12(arg1)
                }
                
                //check arg2
                if(!Entity_NumInCK12.contains(arg2)) {
                  arg2SizeInCK12 = MYSearch.runSearch(CK12IndexPath, "\""+arg2+"\"", "sen", List("sen"), 1000000).size
                  Entity_NumInCK12.put(arg2, arg2SizeInCK12)
                }else {
                  arg2SizeInCK12 = Entity_NumInCK12(arg2)
                }
              }
            }
          }
        }
      }
    }
  }
}