package org.allenai.relation.processors

import org.allenai.relation.util.Searching
import java.io.BufferedReader
import java.io.InputStreamReader


object DemoInstanceSearch {
  private val indexPath: String = "/Users/qingqingcai/Documents/scala/workspace/relationextractionbasedermine/data/test/Index"
  private val search: Searching = new Searching()
  private val maxkptime = 3
  def main(args: Array[String]) = {
    //query=key-phrase ==> ("arg1", "arg2")
    //query=("arg1", "arg2") ==> list-of-sentences
    //query=learned-pattern ==> example-sentences

    println("Input: discourse-relation and lexical-cue-seed (relation-phrase)")
    println("Output: Instances (entity pairs)")
    println()
    
//    var scanner = new Scanner(System.in)
//    var discourse = scanner.nextLine()
//    try{
//      println(discourse)
//    } catch {
//      
//      case ioe: IOException => print("here")
//    }
//    println("done")
    var br:BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    
    
    var discourse = Console.readLine("Discouse relation (FUNCTION; CAUSE; EXAMPLE; ENABLE; PURPOSE): ")
    var relationphrase = Console.readLine("relation phrase> ")
    while (!discourse.equals("-1") && !relationphrase.equals("-1")) {
      var inslist: List[List[String]] = search.runSearch(indexPath, "\"" + relationphrase + "\"", "kp", List("arg1", "arg2", "kp"), 1000)
      println("results for \"" + relationphrase + "\":")
      inslist.foreach(p => println(p(0) + "\t\t" + p(1) + "\t\t" + p(2)))
      println()
      println()
      println()
      discourse = Console.readLine("Discouse relation (FUNCTION; CAUSE; EXAMPLE; ENABLE; PURPOSE): ")
      relationphrase = Console.readLine("key phrase> ")       
    }
  }
}