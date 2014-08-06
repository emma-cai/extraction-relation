package org.allenai.relation.processors

import org.allenai.relation.util.Searching
import scala.collection.mutable.Map

object DemoSentenceSearch {
  private val indexPath: String = "/Users/qingqingcai/Documents/scala/workspace/relationextractionbasedermine/data/test/Index"
  private val search: Searching = new Searching()
  private val maxkptime = 3
  def main(args: Array[String]) = {
    //query=key-phrase ==> ("arg1", "arg2")
    //query=("arg1", "arg2") ==> list-of-sentences
    //query=learned-pattern ==> example-sentences

    println("Input: discourse-relation, argument1 and argument2")
    println("Output: sentences containing both argument1 and argument2")
    println()
    var discourse = Console.readLine("Discouse relation (FUNCTION; CAUSE; EXAMPLE; ENABLE; PURPOSE): ")
    var arg1 = Console.readLine("first argument> ")
    var arg2 = Console.readLine("second argument> ")
    while (!discourse.equals("-1") && !arg1.equals("-1") && !arg2.equals("-1")) {
      var kp_sen: List[List[String]] = search.runSearch(indexPath, "\"" + arg1 + "\"", "\"" + arg2 + "\"", "arg1", "arg2", List("kp", "sen"), 100)
        var kp_num: Map[String, Int] = collection.mutable.Map.empty[String, Int]
        var kp_sen_reduced: List[List[String]] = List()
        kp_sen.foreach {
          case p => {
            val kp = p(0)
            val sen = p(1)
            if (kpcheck(kp, kp_num)) {
              kp_sen_reduced = kp_sen_reduced :::kp_sen_reduced ::: List(List(kp, sen))
            }
          }
        }

        println("results for \"" + arg1 + "\" and \"" + arg2 + "\":")
        kp_sen_reduced.foreach(p => println("\"" + p(0) + "\"" + "\t\t" + p(1)))
        println()
        println()
        println()
        
        discourse = Console.readLine("Discouse relation (FUNCTION; CAUSE; EXAMPLE; ENABLE; PURPOSE): ")
        arg1 = Console.readLine("\n\nfirst argument> ")
        arg2 = Console.readLine("second argument> ")
    }
  }

  def kpcheck(kp: String, kp_num: Map[String, Int]): Boolean = {
    if (!kp_num.contains(kp)) {
      kp_num.put(kp, 1)
      return true;
    } else {
      if (kp_num(kp) < maxkptime) {
        kp_num.put(kp, kp_num(kp) + 1)
        return true;
      } else {
        return false;
      }
    }
  }
}