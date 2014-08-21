package org.qingqing.relation.processors

import org.qingqing.relation.util.Searching
import scala.collection.mutable.Map
import scala.math._

object PhraseSelection {

  private val ReverbIndexPath = "/Users/qingqingcai/Documents/Data/Reverb/Index"
  private val CK12IndexPath = "/Users/qingqingcai/Documents/Data/CK12/Index"
  private val search: Searching = new Searching()
  private val maxins = 1000
  private val maxkptime = 1000
  private var rp_isInReverb = collection.mutable.Map.empty[String, Int]
  private var rp_isInCK12 = collection.mutable.Map.empty[String, Int]
  private var ins_isInReverb = collection.mutable.Map.empty[String, List[List[String]]]

  def main(args: Array[String]) = {
    version2()
  }
  def version2() = {
    val disrel_keyphrase = Map("FUNCTION" -> "used to",
      "PURPOSE" -> "purpose",
      "ENABLE" -> "need",
      "EXAMPLE" -> "an example of", "CAUSE" -> "caused")

    disrel_keyphrase.foreach {
      case (discourse, keyphrase) => {
        println("----------------------------------------------------")
        println(discourse + "\t" + keyphrase)
        var rp_insContained = collection.mutable.Map.empty[String, Int]
        var rp_numInCK12 = collection.mutable.Map.empty[String, Int]
        val inslist: List[List[String]] = search.runSearch(ReverbIndexPath, "\"" + keyphrase + "\"", "kp", List("arg1", "arg2"), maxins)

        var i = 1
        inslist.foreach {
          case ins => {
            val arg1 = ins(0)
            val arg2 = ins(1)
            var relphraselist: List[List[String]] = List()

            //to save the searching time, store the processed instance to map, and get it directly next time
            if (!ins_isInReverb.contains(arg1 + "; " + arg2)) {
              relphraselist = search.runSearch(ReverbIndexPath, "\"" + arg1 + "\"", "\"" + arg2 + "\"", "arg1", "arg2", List("kp"), maxkptime)
              ins_isInReverb.put(arg1 + "; " + arg2, relphraselist)
            } else {
              relphraselist = ins_isInReverb(arg1 + "; " + arg2)
            }

            //for each relation-phrase connecting arg1 and arg2
            relphraselist.foreach {
              case relphrase => {
                //how many instances 
                val rp = relphrase(0)
                if (rp_insContained.contains(rp)) { val old = rp_insContained(rp); rp_insContained.put(rp, old + 1) }
                else rp_insContained.put(rp, 1)

                //how many times this relation-phrase appears in CK12
                var numInCK12 = 0
                if (rp_isInCK12.contains(rp))
                  numInCK12 = rp_isInCK12(rp)
                else {
                  numInCK12 = search.runSearch(CK12IndexPath, rp, "sen", List("sen"), maxkptime).size
                  rp_isInCK12.put(rp, numInCK12)
                }
                if (rp_numInCK12.contains(rp)) { /**val old = rp_numInCK12(rp); rp_numInCK12.put(rp, old+numInCK12)**/ }
                else rp_numInCK12.put(rp, numInCK12)
              }
            }
            if (i % 200 == 0)
              println(i)
            i = i + 1
          }
        }

        //    println("relation-phrase"+"\t"+"ranking-in-reverb"+"\t"+"times-in-reverb"+"\t"+"fre-in-reverb"+"\t"+"ranking-in-ck12"+"\t"+"times-in-ck12"+"\t"+"fre-in-ck12")
        val sum = rp_insContained.foldLeft(0)(_ + _._2)
        val rp_insContainedsorted = rp_insContained.toList.sortWith((x, y) => x._2 > y._2)
        //    rp_insContainedsorted.foreach(p => println(p._1 + "\t" + p._2 + "\t" + (p._2*0.1)/(sum*0.1)))

        val sum_2 = rp_numInCK12.foldLeft(0)(_ + _._2)
        val rp_numInCK12sorted = rp_numInCK12.toList.sortWith((x, y) => x._2 > y._2)
        //    rp_numInCK12sorted.foreach(p => println(p._1 + "\t" + p._2 + "\t" + (p._2*0.1)/(sum_2*0.1)))

        rp_insContainedsorted.foreach(p => {
          val fre_in_reverb = ((p._2 * 0.1) / (sum * 0.1))
          val fre_in_ck12 = ((rp_numInCK12(p._1) * 0.1) / (sum_2 * 0.1))
          println(p._1 + "\t" + p._2 + "\t" + fre_in_reverb + "\t"
            + rp_numInCK12(p._1) + "\t" + fre_in_ck12)
        })
      }
    }
  }

  def version1() = {
    val discourse = "CAUSE"
    val keyphrase = "caused by"
    var rp_numInReverb = collection.mutable.Map.empty[String, Int]
    var rp_numInCK12 = collection.mutable.Map.empty[String, Int]
    val inslist: List[List[String]] = search.runSearch(ReverbIndexPath, "\"" + keyphrase + "\"", "kp", List("arg1", "arg2"), maxins)
    println("ins = " + inslist.size)
    inslist.foreach {
      case ins => {
        println(ins)
        val arg1 = ins(0)
        val arg2 = ins(1)
        val relphraselist = search.runSearch(ReverbIndexPath, "\"" + arg1 + "\"", "\"" + arg2 + "\"", "arg1", "arg2", List("kp"), maxkptime)

        relphraselist.foreach {
          case relphrase => {
            //how many times this relation-phrase appears in Reverb
            val rp = relphrase(0)
            println("\t" + rp)
            var numInReverb = 0
            if (rp_isInReverb.contains(rp))
              numInReverb = rp_isInReverb(rp)
            else {
              numInReverb = search.runSearch(ReverbIndexPath, rp, "kp", List("sen"), maxins).size
              rp_isInReverb.put(rp, numInReverb)
            }
            if (rp_numInReverb.contains(rp)) { val old = rp_numInReverb(rp); rp_numInReverb.put(rp, old + numInReverb) }
            else rp_numInReverb.put(rp, numInReverb)
            println("reverb num for " + rp + ": " + rp_numInReverb)

            //   println(rp_numInReverb)
            //how many times this relation-phrase appears in CK12
            var numInCK12 = 0
            if (rp_isInCK12.contains(rp))
              numInCK12 = rp_isInCK12(rp)
            else {
              numInCK12 = search.runSearch(CK12IndexPath, rp, "sen", List("sen"), maxkptime).size
              rp_isInCK12.put(rp, numInCK12)
            }
            if (rp_numInCK12.contains(rp)) { /**val old = rp_numInCK12(rp); rp_numInCK12.put(rp, old+numInCK12)**/ }
            else rp_numInCK12.put(rp, numInCK12)
            println("ck12 num for " + rp + ": " + rp_numInCK12)
            //   println(rp_numInCK12)
          }
        }
        println(rp_numInReverb)
        println(rp_numInCK12)
      }
    }

    println("(rel-phrase, numInReverb)")
    rp_numInReverb.foreach(println)
    println("------------------------------------")
    println()
    println("(rel-phrase, numInCK12)")
    rp_numInCK12.foreach(println)
  }
}