package org.allenai.relation.preprocess

object pipeline {
  def main(args: Array[String]) = {
    
    // get testing data with annotations
//    val inputpath = "/Users/qingqingcai/Documents/Data/NY_Training/NY_Training_Data.xlsx"
//    val outputpath = "/Users/qingqingcai/Documents/Data/NY_Training/NY_DISREL_Training.txt"
//    val MYTDT = new SentenceDisrelLabelExtraction()
//    MYTDT.readExcel(inputpath, outputpath)
    
    // get testing data without annotations
    val inputpath = "/Users/qingqingcai/Documents/Data/NY_Training/108Q_sentence_arg1_arg2.xlsx"
    val outputpath = "/Users/qingqingcai/Documents/Data/NY_Training/108Q_sentence_arg1_arg2.txt"
    val MYTDT = new SentenceDisrelNoLabelExtraction()
    MYTDT.readExcel(inputpath, outputpath)
  }
}