package org.allenai.relation.preprocess


object pipeline {
  def main(args: Array[String]) = {
	  val inputpath = "/Users/qingqingcai/Documents/Data/NY_Training/NY_Training_Data.xlsx"
	  val outputpath = "/Users/qingqingcai/Documents/Data/NY_Training/NY_DISREL_Training.txt"
	  val MYTDT = new TrainingDataExtraction()
	  MYTDT.readExcel(inputpath, outputpath)
  }
}