package org.allenai.relation.learning

import java.io.File
import org.allenai.relation.util.Polyparser
import scala.collection.mutable.Map
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.collection.immutable.ListMap
import org.allenai.ari.solvers.utils.Tokenizer

object Test {
  def main(args: Array[String]) = {
//    val sentence = "Mechanical energy exerted by one object can push or pull another object."
//    val arg1str = "Mechanical energy"
//    val arg2str = "push or pull another object"
    
    val sentence = "This research , known originally as the Bristol Wearable Computing Initiative , led to participation in the six-year Equator Interdisciplinary Research Collaboration ( IRC ) supported by the EPSRC."
    val arg1str = "research"
    val arg2str = "led"
    val (root, tree) = Polyparser.processText(sentence)
    val arg1list = Polyparser.findHeadW(tree.vertices.toList, arg1str, tree.edges.toList)
    val arg2list = Polyparser.findHeadW(tree.vertices.toList, arg2str, tree.edges.toList)
    
    println("vertices: ");
    tree.vertices.foreach(p => println(p))
    println()
    
    println("edges: ")
    tree.edges.foreach(p => println(p))
    println()
    
    println("arg1list = " + arg1list)
    println("arg2list = " + arg2list)
    println()
    
    println("dependency paths = 4")
    val res4 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 4).toSet
    res4.foreach(p => println(p))
    println()
    
    println("dependency paths = 3")
    val res3 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 3)
    res3.foreach(p => println(p))
    println()
    
    println("dependency paths = 2")
    val res2 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 2)
    res2.foreach(p => println(p))
    println()
    
    println("dependency paths = 1")
    val res1 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 1)
    res1.foreach(p => println(p))
    
    
    
    //test entailment computation
//	  val sentence= "Mechanical energy exerted by one object can push or pull another object."
//	  val sentenceset = Tokenizer.toKeywords(sentence).toSet
//	  println("sentence = " + sentence)
//	  println("sentenceset = " + sentenceset)
//	  println("sentencemkstring = " + sentenceset.mkString(" "))
//	  val disrelSeeds = FeatureWrapper.loadLexicalSeeds("data/learning/train/lexical.seed")
//	  val lexicallist = for {
//          lexicalseed <- disrelSeeds("example")
//        } yield {
//          Tokenizer.toKeywords(lexicalseed).toSet.mkString(" ")
//        }
//	  
//      println("original list = " + disrelSeeds("example"))
//	  
//	  val avg1 = FeatureWrapper.wordnetEntailment(sentenceset.mkString(" "), lexicallist)
//	  println(lexicallist + "\tavg1 = " + avg1)
//	  
//	  
//	  println()
//	  println("=========================================================")
//	  println()
//	  
//	  val avg2 = FeatureWrapper.wordnetEntailment(sentenceset.mkString(" "), lexicallist.mkString(" "))
//	  println(lexicallist + "\tavg2 = " + avg2)
//	  
//	  println()
//	  println("=========================================================")
//	  println()
//	  
//	  lexicallist.foreach(p => {
//	    println(p + "\tavg3 = " + FeatureWrapper.wordnetEntailment(sentenceset.mkString(" "), p))
//	  })
    
  }
  
  def updateMap(map: Map[String, Map[String, Int]], key1: String, key2: String) = {
    var newmap:Map[String, Map[String, Int]] = map
    if(newmap.contains(key1)) {
      if(newmap(key1).contains(key2)) {
        newmap(key1)(key2) = newmap(key1)(key2) + 1
      } else {
        newmap(key1).put(key2, 1)
      }
    } else {
      newmap.put(key1, Map(key2 -> 1))
    }
    newmap
  }
}