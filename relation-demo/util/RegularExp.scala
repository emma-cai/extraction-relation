package org.qingqing.relation.util

class RegularExp {
  def findAllSubstrings(line:String, begLabel:String, endLabel:String):Set[String] = {
    var substrings:Set[String] = Set()
    var str = line
    var begIdx = str.indexOf(begLabel)
	var endIdx = str.indexOf(endLabel, begIdx+1)
	while(begIdx>=0 && endIdx>=0 && endIdx>begIdx) {
	  val w = str.substring(begIdx+1, endIdx)
	  if(w!=null && !w.equals(""))
	    substrings = substrings + w
	  str = str.substring(endIdx+1, str.length())
	  begIdx = str.indexOf("\"")
	  endIdx = str.indexOf("\"", begIdx+1)
	}
    return substrings
  }
}