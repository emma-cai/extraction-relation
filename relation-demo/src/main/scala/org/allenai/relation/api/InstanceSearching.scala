package org.allenai.relation.api

import org.allenai.relation.util.Searching
import org.allenai.relation.util.Write
import scala.collection.mutable.Map

class InstanceSearching {
  private val stopwordsList = List("is", "am", "are", "was", "were", "be", "has", "have", "had", "the", 
      "a", "an", "having", "being", "do", "did", "done", "doing", "does", "your", "you", "me", "my", "mine", "me", 
      "he", "his", "him", "she", "her", "they", "their", "them", "one", "two", "three", "all", "every", "each", 
      "go", "going", "went", "gone", "some", "any")

//  private val maxinsres = 10000
//  private val maxkpres = 10000
//  private val maxkptime = 3
//  private val maxNumOfGood = 300
//  private val minimumInCK12ForRP = 10
//  private val minimumInCK12ForArg1 = 10
//  private val minimumInCK12ForArg2 = 10
      
  private val maxinsres = 1000
  private val maxkpres = 1000
  private val maxkptime = 3
  private val minimumInCK12ForRP = 10
  private val minimumInCK12ForArg1 = 10
  private val minimumInCK12ForArg2 = 10
  
  private val ReverbIndexPath = "/Users/qingqingcai/Documents/Data/Reverb/Index"
  private val entity_countInCK12_path = "/Users/qingqingcai/Documents/Aristo/extraction-new/data/entity_counts_ck12/entity_countsInCK12.txt"
  private val relpha_countInReverbAndCK12_path = "/Users/qingqingcai/Documents/Aristo/extraction-new/data/relationphrase_counts_frequency"
  private val entity_countInCK12 = readMap(entity_countInCK12_path)
  
  /** The discouse-relation lexical cue seeds are defined here;
    * for each discourse relation, find all instances which are connected by the lexical-cue-seed
    */
  def insSearch(indexPath: String, disrel:String, seed:String): List[List[String]] = {
    
    var insList:List[List[String]] = List()
    val MYSearch: Searching = new Searching()
    val perseed = MYSearch.runSearch(indexPath, "\"" + seed + "\"", "kp", List("arg1", "arg2"), maxinsres)
    perseed.foreach {
      case p => {
        if(inscheck(entity_countInCK12, p(0), minimumInCK12ForArg1, p(1), minimumInCK12ForArg2)==true && !insList.contains(perseed))
          insList = insList ::: List(p)
      }
    }
    return insList
  }


  /** The query should contain good words
    */
  def checkquery(beg: String, end: String): Boolean = {
    if (beg.matches(".*[0-9*+=$%&#'/.?].*") || end.matches(".*[0-9*+=$%&#'/.?].*")
      || beg.matches("[A-Za-z]") || end.matches("[A-Za-z]"))
      return false
    if (beg.matches("A man"))
      return false
    return true
  }

  /**
   * check if kp should be added
   * for each discourse-relation, we only consider at most 50 relation-phrases
   * and these relation-phrases should be (kind of) popular in CK12
   */
  def kpcheck(kp: String, kp_num: Map[String, Int], goodRelationPhrase:List[String]): Boolean = {
    if(goodRelationPhrase.contains(kp)) {
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
    return false
  }
  
  /**
   * get goodRelationPhrase from learnt distribution of relation-phrase in both Reverb and CK12
   */
  def selectGoodRelationPhrase(dir:String, maxNumOfGood:Int, minimumOccurInCK12: Int):Map[String, List[String]] = {
    var disrel_goodRelationPhrase = collection.mutable.Map.empty[String, List[String]]
    
    val files = new java.io.File(dir).listFiles.filter(_.getName.endsWith("txt") == true)
    for (file <- files) { //for each file
      var goodRelationPhrase:List[String] = List()
      var filename = file.getName()
      var disrel = filename.substring(0, filename.indexOf("-"))
      var count = 1
	  for(line <- scala.io.Source.fromFile(dir+"/"+filename).getLines()) {
	    val arr = line.split("\t")
	    val relphrase = arr(0)
	    val countInCK12 = arr(3).toInt
	    //the goodRelationPhrase must satisfy: 
	    // (1) frequent when searching using our entity seeds;
	    // (2) is a frequent phrase in CK12 (countInCK12>=minimumOccurInCK12)
	    // (3) cannot only contain stopwords
	    if(count<=maxNumOfGood && countInCK12>=minimumOccurInCK12 
	        && !stopwordsList.contains(relphrase.toLowerCase())) {
	      goodRelationPhrase = goodRelationPhrase:::List(relphrase)
	      count = count+1
	    }
	  }
      disrel_goodRelationPhrase.put(disrel, goodRelationPhrase)
    }
    return disrel_goodRelationPhrase
  }
  
  /**
   * 
   */
  def readMap(source:String):Map[String, Int] = {
    var entity_countInCK12 = collection.mutable.Map.empty[String, Int]
    for(line <- scala.io.Source.fromFile(source).getLines()) {
      val arr = line.split("\t")
      val entity = arr(0)
      val count = arr(1)
      entity_countInCK12.put(entity, Integer.parseInt(count))
    }
    return entity_countInCK12
  }
  
  def inscheck(entity_countInCK12:Map[String, Int], arg1:String, minimumNumForArg1:Int, 
      arg2:String, minimumNumForArg2:Int): Boolean = {
    if(!entity_countInCK12.contains(arg1) || !entity_countInCK12.contains(arg2))
      return false;
    val arg1NumInCK12 = entity_countInCK12(arg1)
    val arg2NumInCK12 = entity_countInCK12(arg2)
    if(arg1NumInCK12>=minimumNumForArg1 
        && arg2NumInCK12>=minimumNumForArg2 
        && !stopwordsList.contains(arg1.toLowerCase()) 
        && !stopwordsList.contains(arg2.toLowerCase())) {
      return true
    }
    return false
  }
  
  def sencheck(sen:String, arg1:String, arg2:String): (Boolean, String) = {
    if(sen.matches(".*"+arg1+".*")==false || sen.matches(".*"+arg2+".*")==false)
      return (false, null)
    val index = sen.lastIndexOf(":")
    if(index != -1) {
      val newsen = sen.substring(index+1, sen.length())
      if(newsen.matches(".*"+arg1+".*") && newsen.matches(".*"+arg2+".*"))
        return (true, newsen)
      else
        return (false, null)
    }
    return (true, sen)
  }
}