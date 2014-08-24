package org.allenai.relation.test

import scala.collection.mutable.Map
import scala.io.Source

object GetFeaturesAndSentenceByID {
  def main(args: Array[String]) = {
    val id = "853"
    val idfeaturemaps = read("data/binary/arff/test.arff")
    val idsentencemaps = read("data/binary/inputDirectory/108Q_sentence_arg1_arg2.txt")
    val feature = idfeaturemaps(id)
    val sentence = idsentencemaps(id)

    println(id)
    println(feature)
    println(sentence)
  }

  def read(ifilepath: String) = {
    var maps: Map[String, String] = collection.mutable.Map.empty
    var i = 1
    Source.fromFile(ifilepath).getLines().foreach {
      line =>
        {
          if (!line.startsWith("@") && !line.startsWith("{") && !line.startsWith(" ") && !line.equals("") && !line.startsWith("sid")) {
            maps.put(i.toString, line)
            i = i + 1
          }
        }
    }
    maps
  }
}