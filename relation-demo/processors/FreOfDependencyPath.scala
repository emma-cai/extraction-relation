package org.qingqing.relation.processors

import scala.collection.mutable.Map

object FreOfDependencyPath {
  def main(args: Array[String]) = {
   // val source = "/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_dp"
    val source = "/Users/qingqingcai/Documents/Data/Barrons/experiments/disrel_tuples_dp"
    var disrel_dps_num = readFromFile(source)

    disrel_dps_num.foreach {
      case (disrel, dps_num) => {
        println(disrel)
        //       println(dps_num)
        val sum = dps_num.foldLeft(0)(_ + _._2)
        val dps_num_sorted = dps_num.toList.sortWith((x, y) => x._2 > y._2)
        dps_num_sorted.foreach {
          case (dp, num) => {
            var dpstr: String = dp.head

            for (e <- dp) {
              if (!e.equals(dp.head))
                dpstr = dpstr + " & " + e
            }

            println(dpstr + "\t" + num /** + "\t" + ((num*0.1)/(sum*0.1))**/ )
          }
        }
        println()
      }
    }
  }

  /** Store tuples in a file, and get frequency from the file (considering the efficiency)
    */
  def readFromFile(source: String): Map[String, Map[Set[String], Int]] = {
    var disrel_dps_num: Map[String, Map[Set[String], Int]] = collection.mutable.Map.empty[String, Map[Set[String], Int]]
    val files = new java.io.File(source).listFiles.filter(_.getName.endsWith(".txt"))
    for (file <- files) {
      // process the file
      var filename = file.getName()
      for (line <- scala.io.Source.fromFile(file).getLines()) {
        //for each line, get two columns(disrel, dpslist)
        var tuple = getDPsList(line)
        var disrel = tuple._1
        var dpslist = tuple._2
        var rahformat = convertRaphaelFormat(dpslist)
        //update the hashmap
        if (rahformat != null)
          rahformat.foreach(dp => update(disrel_dps_num, disrel, dp))
      }
    }

    return disrel_dps_num
  }

  /** If disrel never appears in map, add it, and (dp, num) is initialized to be 1;
    * If disrel appears before, check dp in (dp, num)
    */
  def update(disrel_dps_num: Map[String, Map[Set[String], Int]], disrel: String, dp: Set[String]) = {
    if (disrel_dps_num.contains(disrel)) {
      val dps_num: Map[Set[String], Int] = disrel_dps_num(disrel)
      if (dps_num.contains(dp)) {
        val tmp = dps_num(dp)
        dps_num.put(dp, tmp + 1)
      } else {
        dps_num.put(dp, 1)
      }
    } else {
      disrel_dps_num.put(disrel, Map(dp -> 1))
    }
  }

  /** For each Set[Set[String]], represented in string, convert it to list
    * eg: Set(Set(nsubjpass(_, _1), det(_, _2), prep_for(_, _)), Set(nsubjpass(_, _1), amod(_, _2), prep_for(_, _)), Set(nsubjpass(_, _1), prep_for(_, _2)))
    * converted to: List(Set1, Set2, Set3, ..., SetN)
    */
  def getDPsList(line: String): (String, List[Set[String]]) = {
    var dpslist: List[Set[String]] = List()
    val linearr = line.split("\t")
    val disrel = linearr(0)
    var dpstr = linearr(linearr.length - 1)
    dpstr = dpstr.substring(4, dpstr.length() - 1)

    var beg = dpstr.indexOf("Set(")
    var end = dpstr.indexOf("), Set")
    var tmp = dpstr
    while (beg >= 0 && end > 0 && end > beg) {
      var onesetstr = tmp.substring(beg, end + 1)
      onesetstr = onesetstr.substring(4, onesetstr.length() - 1)
      var oneset = stringtoset(onesetstr, "\\), ")
      if (oneset != null && oneset.size > 0)
        dpslist = dpslist ::: List(oneset)
      tmp = tmp.substring(end + 3)
      beg = tmp.indexOf("Set(")
      end = tmp.indexOf("), Set")
    }
    if (tmp != null && !tmp.equals(""))
      dpslist = dpslist ::: List(stringtoset(tmp.substring(4, tmp.length() - 1), "\\), "))
    return (disrel, dpslist)
  }

  def stringtoset(str: String, split: String): Set[String] = {

    var setv: Set[String] = Set()
    val arr = str.split(split)
    for (i <- 0 to arr.length - 2) {
      var na = arr(i) + ")"
      setv = setv ++ Set(na)
    }
    setv = setv ++ Set(arr(arr.length - 1))
    return setv
  }

  def convertRaphaelFormat(dpslist: List[Set[String]]): List[Set[String]] = {
    var newdpslist: List[Set[String]] = List()
    dpslist.foreach {
      case eachset => {
        var neweachset: Set[String] = Set()
        eachset.foreach {
          case ele => {
            var newele = ele
            newele = newele.replaceAll("_1", "a")
            newele = newele.replaceAll("_2", "b")
            newele = newele.replaceAll("_3", "c")
            newele = newele.replaceAll("_4", "d")
            newele = newele.replaceAll("_5", "e")
            newele = newele.replaceAll("_6", "f")

            val index1 = newele.indexOf("(")
            val index2 = newele.indexOf(",")
            val index3 = newele.indexOf(")")
            val dis = newele.substring(0, index1)
            val arg1 = newele.substring(index1 + 1, index2)
            val arg2 = newele.substring(index2 + 2, index3)
            var newdis = ""
            if (!dis.startsWith("token:")) {
              newdis = "dep(" + arg1 + ", " + "\"" + dis + "\", " + arg2 + ")"
            } else {
              newdis = dis.substring(dis.indexOf(":") + 1) + "(" + arg1 + ", " + "\'" + arg2 + "\')"
            }
            neweachset = neweachset ++ Set(newdis)
          }
        }
        newdpslist = newdpslist ::: List(neweachset)
      }
    }
    return newdpslist
  }
}