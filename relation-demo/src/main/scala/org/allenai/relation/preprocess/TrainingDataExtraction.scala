package org.allenai.relation.preprocess

import java.io._

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import scala.collection.mutable.Map
import org.allenai.relation.util.Write

class TrainingDataExtraction {

  /** inputpath: NY Training data
    * outputpathdata: id, sen, disrel, arg1, arg2, label
    * outputpathlcfre: disrel, lexical-cue, count, frequency
    */

  val idIndex = 0
  val tfqueIndex = 2
  val tfqueruleIndex = 3
  val senIndex = 5
  val senruleIndex = 6
  var existed: List[String] = List()

  def run(inputpath: String, outputpathdata: String, outputpathlcfre: String) {

  }

  def readExcel(inputpath: String, outputpath: String) = {
    var tuplelist: List[(String, String, String, String, String, String)] = List()
    try {
      val file: FileInputStream = new FileInputStream(new File(inputpath))
      val workbook: XSSFWorkbook = new XSSFWorkbook(file)
      val sheet: XSSFSheet = workbook.getSheetAt(0)

      val rowIterator = sheet.rowIterator()
      var id = 0
      while (rowIterator.hasNext()) {
        val row = rowIterator.next()
        if (row.getCell(idIndex) != null) {

          //          val tfque = row.getCell(tfqueIndex).toString()
          //          val tfquerule = row.getCell(tfqueruleIndex).toString()
          //          var infolist = getInfo(tfquerule)
          //          if(infolist!=null && infolist.size > 0) {
          //            infolist.foreach {
          //              case info => {
          //                id = id+1
          //                tuplelist = tuplelist ::: List((id.toString, tfque, info._1, info._2, info._3, "1"))
          //              }
          //            } 
          //          }

          val sen = row.getCell(senIndex).toString()
          val senrule = row.getCell(senruleIndex).toString()
          var infolist = getInfo(senrule)
          if (infolist != null && infolist.size > 0) {
            infolist.foreach {
              case info => {
                id = id + 1;
                tuplelist = tuplelist ::: List((id.toString, sen, info._1, info._2, info._3, "1"))
              }
            }
          }
        }
      }

    } catch {
      case e: Exception => e.printStackTrace()
    }
    tuplelist.foreach(println)
    val MYWrite = new Write()
    MYWrite.rewrite_3(outputpath, tuplelist)
  }

  def getInfo(rule: String): List[(String, String, String)] = {
    //    barrons.rule1874:: 
    //    isa(E9S786-change, "change"), 
    //    agent(E9S786-change, A1S786-melt), 
    //    isa(A1S786-melt, "Melting"), 
    //    object(E9S786-change, A10S786-water), 
    //    isa(A10S786-water, "water"), 
    //    from(E9S786-change, A14S786-state), 
    //    isa(A14S786-state, "its[Melting] solid state"), 
    //    to(E9S786-change, A21S786-state), 
    //    isa(A21S786-state, "a liquid state") 
    //    -> 
    //    enable(E24S786-addition, E9S786-change), 
    //    isa(E24S786-addition, "add"), 
    //    agent(E24S786-addition, A1S786-melt), 
    //    object(E24S786-addition, A26S786-heat), 
    //    isa(A26S786-heat, "heat").
    var tuplelist: List[(String, String, String)] = List()
    var rel_arg1_arg2: Map[String, Map[String, String]] = collection.mutable.Map.empty[String, Map[String, String]]

    if (rule.startsWith("None") || rule.startsWith("none")
      || rule.startsWith("Not extracted") || rule.startsWith("NA"))
      return null

    var rule_mod = rule;
    if (rule.startsWith("barrons"))
      rule_mod = rule.substring(rule.indexOf("::") + 3, rule.length() - 1)
    val tmp = rule_mod.split(" -> ")
    if (tmp.size != 2)
      return null

    tmp.foreach {
      case p => {
        val rule_arr = p.trim().split("\\)")
        rule_arr.foreach {
          case r => {
            val index1 = r.indexOf("(")
            val index2 = r.indexOf(", ", index1)
            val index3 = r.length()
            if (index1 > 0 && index2 > index1 && index3 > index2) {
              var rel = ""
              if (r.startsWith(","))
                rel = r.substring(2, index1).trim()
              else
                rel = r.substring(0, index1).trim()
              val arg1 = r.substring(index1 + 1, index2).trim()
              val arg2 = r.substring(index2 + 2, index3).trim()
              if (rel_arg1_arg2.contains(rel)) {
                var arg1_arg2: Map[String, String] = rel_arg1_arg2(rel)
                if (rel.equals("isa")) {
                  if (arg2.startsWith("\""))
                    arg1_arg2.put(arg1, arg2)
                } else {
                  arg1_arg2.put(arg1, arg2)
                }
                rel_arg1_arg2.put(rel, arg1_arg2)
              } else {
                var arg1_arg2: Map[String, String] = collection.mutable.Map.empty[String, String]
                if (rel.equals("isa")) {
                  if (arg2.startsWith("\""))
                    arg1_arg2.put(arg1, arg2)
                } else {
                  arg1_arg2.put(arg1, arg2)
                }

                rel_arg1_arg2.put(rel, arg1_arg2)
              }
            }
          }
        }
      }
    }
    //   rel_arg1_arg2.foreach(println)

    rel_arg1_arg2.filter(p => !p._1.equals("agent")
      && !p._1.equals("object") && !p._1.equals("isa")
      && !p._1.equals("for") && !p._1.equals("into")
      && !p._1.equals("from") && !p._1.equals("to")
      && !p._1.equals("arg") && !p._1.equals("through")
      && !p._1.equals("over") && !p._1.equals("when")
      && !p._1.equals("in") && !p._1.equals("base")
      && !p._1.equals("as") && !p._1.equals("during")
      && !p._1.equals("on") && !p._1.equals("toward")
      && !p._1.equals("with")).foreach {
      case p => {
        val relstr = p._1
        val arg1_arg2s = p._2
        arg1_arg2s.foreach {
          case each => {
            val arg1stem = each._1
            val arg2stem = each._2
            val arg1str = rel_arg1_arg2("isa")(arg1stem)
            val arg2str = rel_arg1_arg2("isa")(arg2stem)
            tuplelist = tuplelist ::: List((relstr, arg1str, arg2str))
          }
        }
      }
    }
    return tuplelist
  }
}