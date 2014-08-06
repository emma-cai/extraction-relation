package org.allenai.relation.processors

import org.allenai.relation.util.Searching
import scala.collection.mutable.Map

object Demo {
  private val indexPath: String = "/Users/qingqingcai/Documents/scala/workspace/relationextractionbasedermine/data/test/Index"
  private val search: Searching = new Searching()
  private val maxkptime = 3
  def main(args: Array[String]) = {
    //query=key-phrase ==> ("arg1", "arg2")
    //query=("arg1", "arg2") ==> list-of-sentences
    //query=learned-pattern ==> example-sentences

    println("1:Instance-Search\t2:Sentence-Search\t3:Pattern-Search\t-1:Quit")
    var choice = readLine("\n\nchoice_id> ")
    while (!choice.equals("-1")) {
      if (choice.equals("1")) {
        var discourse = readLine("Discouse relation (FUNCTION; CAUSE; EXAMPLE; ENABLE; PURPOSE): ")
        var keyphrase = readLine("key phrase> ")
        var inslist: List[List[String]] = search.runSearch(indexPath, "\"" + keyphrase + "\"", "kp", List("arg1", "arg2", "kp"), 1000)
        println("results for \"" + keyphrase + "\":")
        inslist.foreach(p => println(p(0) + "\t\t" + p(1) + "\t\t" + p(2)))
        println()
        println()
        println()
      } else if (choice.equals("2")) {
        var discourse = readLine("Discouse relation (FUNCTION; CAUSE; EXAMPLE; ENABLE; PURPOSE): ")
        var arg1name = readLine("arg1> ")
        var arg2name = readLine("arg2> ")
        var kp_sen: List[List[String]] = search.runSearch(indexPath, "\"" + arg1name + "\"", "\"" + arg2name + "\"", "arg1", "arg2", List("kp", "sen"), 100)
        var kp_num: Map[String, Int] = collection.mutable.Map.empty[String, Int]
        var kp_sen_reduced: List[List[String]] = List()
        kp_sen.foreach {
          case p => {
            val kp = p(0)
            val sen = p(1)
            if (kpcheck(kp, kp_num)) {
              kp_sen_reduced = kp_sen_reduced ::: List(List(kp, sen))
            }
          }
        }

        println("results for \"" + arg1name + "\" and \"" + arg2name + "\":")
        kp_sen_reduced.foreach(p => println("\"" + p(0) + "\"" + "\t\t" + p(1)))
        println()
        println()
        println()
      } else if (choice.equals("3")) {
        //      var discourse = readLine("Discouse relation (1:FUNCTION; 2:CAUSE; 3:EXAMPLE; 4:ENABLE): ")
      } else {
        println("wrong input!")
        println("1:Instance-Search\t2:Sentence-Search\t3:Pattern-Search\t-1:Quit")
        println()
      }
      choice = readLine("choice_id> ")
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