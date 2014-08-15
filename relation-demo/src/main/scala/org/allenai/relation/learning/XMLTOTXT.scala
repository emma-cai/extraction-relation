package org.allenai.relation.learning

import java.io.FileInputStream
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import java.io.File
import org.apache.poi.xssf.usermodel.XSSFSheet
import org.allenai.relation.util.Write

object XMLTOTXT {
  
  // in case of store the data in one file
  private var tuplesalllist: List[(String, String, String, String, String, String, String)] = List()
  private var idall = 0
  
  def main(args: Array[String]):Unit = {
    readExcel("data/binary/xlsx/manually-labeled-data-in-process.xlsx", 
        "data/binary/inputDirectory/all.txt", "all")
  }
  
  def readExcel(inputpath: String, outputDirectory: String, choice: String) = {
    try {
      val file: FileInputStream = new FileInputStream(new File(inputpath))
      val workbook: XSSFWorkbook = new XSSFWorkbook(file)
      (0 to workbook.getNumberOfSheets()-1).map { i =>
        val sheetname = workbook.getSheetName(i)
        
        if(!sheetname.startsWith("Sheet")) {
          println("dealing " + sheetname)
          val sheet:XSSFSheet = workbook.getSheetAt(i)
          
          choice match {
            case "single" => 
              // store separate sheet to single files
              val tuplelist = readSheet(sheet, choice)
              val outputpath = outputDirectory + "/" + sheetname + ".txt"
              val MYWrite = new Write()
              MYWrite.rewrite_4(outputpath, tuplelist)
            case "all" =>
              // store all sheet to one file
              readSheet(sheet, choice)
              val MYWrite = new Write()
              MYWrite.rewrite_4(outputDirectory+"/all.txt", tuplesalllist)
          }
        }
      }
    }catch {
      case e: Exception => e.printStackTrace()
    }
  }
  
  def readSheet(sheet: XSSFSheet, choice: String) = {
    println(sheet)
    val senIndex = 0
    val disrelIndex = 1
    val relphraseIndex = 2
    val arg1Index = 3
    val arg2Index = 4
    val labelIndex = 5
    
    var tuplelist:List[(String, String, String, String, String, String, String)] = List()
    val rowIterator = sheet.rowIterator()
    var id = 0
	while(rowIterator.hasNext()) {
	  val row = rowIterator.next()
	  if(row.getCell(senIndex) != null 
	    && row.getCell(labelIndex) == null
	    && (!row.getCell(senIndex).toString().contains("\t")) 
	    && id<1000) {
	    println(id)
	    id = id+1
	    idall = idall+1
	    choice match {
	      case "single" =>
	        // store single sheet data
	        tuplelist = tuplelist ::: List((id.toString, 
	          row.getCell(senIndex).toString(), 
	          row.getCell(disrelIndex).toString(), 
	          row.getCell(relphraseIndex).toString(), 
	          row.getCell(arg1Index).toString(), 
	          row.getCell(arg2Index).toString(), 
	          ""))
	      case "all" => 
	        // store all sheet data
	        tuplesalllist = tuplesalllist ::: List((idall.toString, 
	          row.getCell(senIndex).toString(), 
	          row.getCell(disrelIndex).toString(), 
	          row.getCell(relphraseIndex).toString(), 
	          row.getCell(arg1Index).toString(), 
	          row.getCell(arg2Index).toString(), 
	          ""))
	    }
      }
	}
    tuplelist
  }  
}