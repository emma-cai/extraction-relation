package org.allenai.relation.learning

import java.io.PrintWriter
import collection.immutable.ListMap
import scala.collection.mutable.Map

object LexicalFrequency extends App {
  val inputFile = "data/learning/train/training.txt"
  val outputFile = "data/learning/train/lexicalfrequency.txt"
  processText(inputFile, outputFile)
  System.exit(0)
  
  def processText(inputFile: String, outputFile: String) = {
    val source = scala.io.Source.fromFile(inputFile)
    
    // map = Map[disrel, Map[lexical, count]]
    var map: Map[String, Map[String, Int]] = collection.mutable.Map.empty[String, Map[String, Int]]
    for(line <- source.getLines.drop(1)) {
      val (sid, sentence, disrel, relphrase, arg1str, arg2str, annotationOpt) = splitToTuple(line, "\t")
      if(annotationOpt.equals("1")) {
    	map = updateMap(map, disrel, relphrase)
	  }
    }
    
    // convert map to disrel_lexicalset_map = Map[disrel, Set[lexical]]
    val disrel_lexicalset_map = for {
      (disrel, lexical_count_map) <- map
    } yield {
      disrel -> lexical_count_map.keySet
    }
    disrel_lexicalset_map.foreach(p => println(p))
    
    // write the lexical cues to file, each line = disrel\tlexical
    val writer = new PrintWriter(outputFile)
    for {
      (disrel, lexicalset) <- disrel_lexicalset_map
      lexical <- lexicalset
    } {
      writer.println(disrel + "\t" + lexical)
    }
    writer.close()
  }
  
  /**
   * Update the map:
   * If key1 and key2 exists, map(key1)(key2) = oldvalue + 1;
   * If key1 exists but not key2, map(key1)(key2) = 1;
   * If key1 not exists, map(key1)(key2) = 1;
   */
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
  
  /**
   * split string into tuples
   */
  def splitToTuple(str: String, regex: String) = {
    str.split(regex) match {
      case Array(str1, str2, str3, str4, str5, str6, str7) => (str1, str2, str3, str4, str5, str6, str7)
      case _ => sys.error("not appropriate colons! should be (sid, sentence, disrel, relphrase, arg1, arg2, annotationOpt)")
    }
  }
}