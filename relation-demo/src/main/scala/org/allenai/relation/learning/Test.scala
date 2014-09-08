package org.allenai.relation.learning

import java.io.File
import org.allenai.relation.util.Polyparser
import scala.collection.mutable.Map
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.collection.immutable.ListMap

object Test {
  def main(args: Array[String]) = {
//    val sentence = "Mechanical energy exerted by one object can push or pull another object."
//    val arg1str = "Mechanical energy"
//    val arg2str = "push or pull another object"
    
//    val sentence = "Sound waves and light energy are similar because they move energy from one place to another."
//    val arg1str = "Sound waves"
//    val arg2str = "move energy"
//    val (root, tree) = Polyparser.processText(sentence)
//    val arg1list = Polyparser.findHeadW(tree.vertices.toList, arg1str, tree.edges.toList)
//    val arg2list = Polyparser.findHeadW(tree.vertices.toList, arg2str, tree.edges.toList)
//    
//    println("vertices: ");
//    tree.vertices.foreach(p => println(p))
//    println()
//    
//    println("edges: ")
//    tree.edges.foreach(p => println(p))
//    println()
//    
//    println("arg1list = " + arg1list)
//    println("arg2list = " + arg2list)
//    println()
//    
//    println("dependency paths = 4")
//    val res4 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 4).toSet
//    res4.foreach(p => println(p))
//    println()
//    
//    println("dependency paths = 3")
//    val res3 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 3)
//    res3.foreach(p => println(p))
//    println()
//    
//    println("dependency paths = 2")
//    val res2 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 2)
//    res2.foreach(p => println(p))
//    println()
//    
//    println("dependency paths = 1")
//    val res1 = FeatureWrapper.getpathwithspecificlength(root, tree, arg1list, arg2list, 1)
//    res1.foreach(p => println(p))
    
	  var map: Map[String, Map[String, Int]] = Map("a" -> Map("b" -> 1, "c"->2), 
	      ("b" -> Map("c" -> 1)))
	  map.foreach(p => println(p))
	  
	  println("add b to a")
	  val map1 = updateMap(map, "a", "b")
	  map1.foreach(p => println(p))
	  
	  println("add d to b")
	  val map2 = updateMap(map, "b", "d")
	  map2.foreach(p => println(p))
	  
	  println("add c to newmap")
	  val map3 = updateMap(map, "c", "m")
	  map3.foreach(p => println(p))
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