package org.allenai.relation.preprocess

import java.io._

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import scala.collection.mutable.Map
import org.allenai.relation.util.Write
import scala.collection.immutable.SortedSet

class SentenceDisrelNoLabelExtraction {

  /** inputpath: NY Training data
    * outputpathdata: id, sen, disrel, arg1, arg2, label
    * outputpathlcfre: disrel, lexical-cue, count, frequency
    */

  val idIndex = 0
  val senIndex = 1
  val arg1Index = 4
  val arg2Index = 5
  var existed: List[String] = List()
  
  var distantRels: Set[String] = Set("purpose", "cause", 
      /**"effect", **/"example", "enable", 
      /**"part",**/ "requirement"/**, "condition"**/)
  
  
  def readExcel(inputpath: String, outputpath: String) = {
    var tupleset: Set[(Int, String, String, String, String, String, String)] = Set()
    try {
      val file: FileInputStream = new FileInputStream(new File(inputpath))
      val workbook: XSSFWorkbook = new XSSFWorkbook(file)
      val sheet: XSSFSheet = workbook.getSheetAt(0)

      val rowIterator = sheet.rowIterator()
      var id:Int = 0
      
      // get distant discourse-relations
      rowIterator.next()		// drop the headline
      while (rowIterator.hasNext()) {
        val row = rowIterator.next()
        if (row.getCell(senIndex) != null 
            && !row.getCell(senIndex).toString().equals("") 
            && !row.getCell(senIndex).toString().equals(" ")) {
          val sen = row.getCell(senIndex).toString()
          val arg1 = row.getCell(arg1Index).toString()
          val arg2 = row.getCell(arg2Index).toString()
          println("\""+sen+"\"" + "\t\t\t" + arg1 + "\t\t\t" + arg2)
          distantRels.foreach{
            case disrel => {
              id = id+1
              tupleset += ((id, sen, disrel, "", arg1, arg2, ""))
            }
          }
        }
      }
      println("id = " + id)
    } catch {
      case e: Exception => e.printStackTrace()
    }
    val MYWrite = new Write()
    MYWrite.rewrite_4(outputpath, ("sid", "sentence", "disrel", "relphrase", "arg1", "arg2", "annotationOpt"), tupleset)
  }
}