package org.allenai.relation.test

object JsonTest {
  def main(args: Array[String]) = {
    var list1 = List(1, 2, 3, 4, 5)
    var list2 = List(2, 4, 5, 6, 9)
    var toTuples = list1.zip(list2)
    println(list1)
    println(list2)
    println(toTuples)
    var list3: List[(String, String)] = List()
    toTuples.foreach(p => list3 = list3 ::: List((("key:" + p._1), ("val:" + p._2))))
    println(list3)
  }
}