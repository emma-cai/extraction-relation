package org.allenai.relation.api

class SentenceSearching {
  /**
   * Input file: CAUSE	is caused by	gas	bacteria	Odor in gas is caused by bacteria
   * Query: disrel, arg1, arg2
   * Output: List[List[]], where the outer is the searching results, inner is searching field (kp, sen)
   */
  def senSearch(dir:String, disrel:String, arg1:String, arg2:String):List[List[String]] = {
    var kp_sen_list:List[List[String]] = List()
    val source = dir + "/" + disrel + ".txt";
    println(source)
    for(line <- scala.io.Source.fromFile(source).getLines()) {
      val arr = line.split("\t")
      if(arr.length == 5) {
        val kpstored = arr(1)
        val arg1stored = arr(2)
        val arg2stored = arr(3)
        val senstored = arr(4)
        if(arg1.equals(arg1stored) && arg2.equals(arg2stored)) {
          var fields:List[String] = List()
          fields = fields ::: List(kpstored)
          fields = fields ::: List(senstored)
          kp_sen_list = kp_sen_list ::: List(fields)
        }
      }
    }
    return kp_sen_list
  }
}