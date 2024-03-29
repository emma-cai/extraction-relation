package org.allenai.relation.learning

import java.io.PrintWriter

object TrainingData extends App {
  val inputFile = "data/bootstrapping/disrel_tuple_labeled/disrel_tuple_labeled.txt"
  val outputFile = "data/classifier/train/training.txt"
  processText(inputFile, outputFile)
  
  def processText(inputFile: String, outputFile: String) = {
    // Source.fromFile(file).getLines().drop(headerLinesToDrop).map {
    val source = scala.io.Source.fromFile(inputFile)
    val writer = new PrintWriter(outputFile)
    writer.println("sid\tsentence\tdisrel\trelphrase\targ1\targ2\tannotationOpt")
    for(line <- source.getLines.drop(1)) {
      val (sid, sentence, disrel, relphrase, arg1, arg2, annotationOpt) = splitToTuple(line, "\t")
      writer.println(sid+"\t"+sentence+"\t"+disrel+"\t"+relphrase+"\t"+arg1+"\t"+(relphrase+" "+arg2)+"\t"+annotationOpt)
    }
    writer.close()
  }
  
  def splitToTuple(str: String, regex: String) = {
    str.split(regex) match {
      case Array(str1, str2, str3, str4, str5, str6, str7) => (str1, str2, str3, str4, str5, str6, str7)
      case _ => sys.error("not appropriate colons! should be (sid, sentence, disrel, relphrase, arg1, arg2, annotationOpt)")
    }
  }
}