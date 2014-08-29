package org.allenai.relation.learning

object Converting extends App {
  val (disrel, pathset) = parseFeature("example => (conj(_3, _1), conj(_3, _2))", " => ")
  println(disrel)
  pathset.foreach(p => println("name = " + p.label + "\n" + "arg1 = " + p.source + "\n" + "arg2 = " + p.dest))
  System.exit(0)
 
  /**
   * Input: example => (conj(_3, _1), conj(_3, _2))     |     split=" => "
   * Output: disrel: String = example
   * 		 pathset: set[Path] = {Path(conj(_3, _1)), Path(conj(_3, _2))}
   */
  def parseFeature(feature: String, split: String): (String, Set[Path]) = {
//    println(feature)
    val splitindex = feature.indexOf(split)
    if(feature.contains("(") && splitindex != -1) {
      val disrel = feature.substring(0, splitindex)
	    val pathstr = feature.substring(splitindex+split.length()+1, feature.length()-1)	// remove the first "(", and the last ")"
	    val pathset = stringtoset(pathstr, "\\), ")			// separate by "), "
	    var pathstructset: Set[Path] = Set()
	    pathset.foreach(p => pathstructset = pathstructset + new Path(p))
	    return (disrel, pathstructset)
    }
    return (null, null)
  }
  
  /**
   * Convert a String to Set
   * Input: String = conj(_3, _1), conj(_3, _2)
   * Output: Set[String] = Set(conj(_3, _1), conj(_3, _2))
   */
  def stringtoset(str: String, split: String) = {
    var setv: Set[String] = Set()
    val arr = str.split(split)
    for (i <- 0 to arr.length - 2) {
      var na = arr(i) + ")"
      setv = setv ++ Set(na)
    }
    setv = setv ++ Set(arr(arr.length - 1))
    setv
  }
}

/**
 * Constructor function for class Path()
 * Input: pathstr = "nn(_3, _1)"
 * After construction: string = "nn"; arg1 = "_3"; arg2 = "_1"
 */
class Path(pathstr: String) {
  val label = pathstr.substring(0, pathstr.indexOf("("))
  val source = pathstr.substring(pathstr.indexOf("(")+1, pathstr.indexOf(", "))
  val dest = pathstr.substring(pathstr.indexOf(", ")+2, pathstr.indexOf(")"))
  
  def tostring = "label = " + label + "; source = " + source + "; dest = " + dest
}