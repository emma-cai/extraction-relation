package org.allenai.relation.processors
import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import org.allenai.relation.util.Polyparser
import org.allenai.relation.util.Write

object RunDependencyPath {
  private val MAXLEN = 4 //the maximum denpendency-path length we considered here is 4
  def main(args: Array[String]) = {
    //    test()
    //    var source = "/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_v2"
    //    var outdir = "/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_dp"
    var source = "/Users/qingqingcai/Documents/Data/Barrons/experiments/disrel_tuples"
    var outdir = "/Users/qingqingcai/Documents/Data/Barrons/experiments/disrel_tuples_dp"
    var MYWrite = new Write()
    var disrel_tuples_dp: Map[String, (String, String, String, String, String, Set[Set[String]])] = collection.mutable.Map.empty[String, (String, String, String, String, String, Set[Set[String]])]
    val MYPolyparser = new Polyparser()
    val files = new java.io.File(source).listFiles.filter(_.getName.endsWith(".txt"))
    for (file <- files) {
      // process the file
      var filename = file.getName()
      var tupleslist: List[(String, String, String, String, String, Set[Set[String]])] = List()
      for (line <- scala.io.Source.fromFile(file).getLines()) {
        var dependencypaths: Set[Set[String]] = Set()
        val linearr = line.split("\t")
        val linetuple = (linearr(0), linearr(1), linearr(2), linearr(3), linearr(4))
        val disrel = linetuple._1
        val kp = linetuple._2
        val arg1name = linetuple._3
        val arg2name = linetuple._4
        val sen = linetuple._5
        println("==================")
        println("sen = " + sen)

        val tree = MYPolyparser.processText(sen)
        if (tree != null) {

          //          val arg1list = MYPolyparser.getid(tree.vertices.toList, arg1name)
          //          val arg2list = MYPolyparser.getid(tree.vertices.toList, arg2name)
          val arg1list = findHeadW(MYPolyparser, tree.vertices.toList, arg1name, tree.edges.toList)
          val arg2list = findHeadW(MYPolyparser, tree.vertices.toList, arg2name, tree.edges.toList)
          //	  println("arg1list = " + arg1list + "\t arg2list = " + arg2list)
          arg1list.foreach(arg1 => arg2list.foreach(arg2 => {
            //	    println("arg1id = " + arg1 +"\t arg2 = " + arg2)
            var dependencypatheach = findDepPath(arg1, arg2, tree.vertices.toList, tree.edges)
            if (dependencypatheach._1 == true && dependencypatheach._2 != null)
              dependencypaths = dependencypaths.union(dependencypatheach._2)
          }))

          //         println("afeter generalization: ")
          //         dependencypaths.foreach(println)
          val newtuples: (String, String, String, String, String, Set[Set[String]]) = (disrel, kp, arg1name, arg2name, sen, dependencypaths)
          //  disrel_tuples_dp.put(disrel, newtuples)
          tupleslist = tupleslist ::: List(newtuples)

        }
      }
      MYWrite.rewrite_2(outdir + "/" + filename, tupleslist)
    }
  }

  def test() = {
    println("here")
    var MYPolyparser = new Polyparser()

    val text = "A cancerous tumor is an example of a lesion,however the surrounding tissue damaged by a tumor is also a lesion."
    println(text)
    var tree = MYPolyparser.processText(text)

    if (tree != null) {
      println("polyparser:")
      for (edge <- tree.edges) {
        println(edge)
      }
    }

    val res1 = findHeadW(MYPolyparser, tree.vertices.toList, "A cancerous tumor", tree.edges.toList)
    println("arg1 = " + res1)
    val res2 = findHeadW(MYPolyparser, tree.vertices.toList, "a lesion", tree.edges.toList)
    println("arg2 = " + res2)
  }

  /** Given nodes-set(tree.nodes), dependencies-set(tree.dependencies), arg1-id and arg2-id
    * find all dependency-paths connecting arg1-id and arg2-id
    */
  def findDepPath(arg1: Int, arg2: Int, ns: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode],
    ds: Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]): (Boolean, Set[Set[String]]) = {
    var dependencypaths: List[Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]] = List()
    for (pl <- 0 to MAXLEN) {
      var forspelen = findDepPathWithSpeLen(ns, ds, arg1, arg2, pl)
      if (forspelen._1 == true && forspelen._2 != null) {
        forspelen._2.foreach(p => if (!dependencypaths.contains(p)) dependencypaths = dependencypaths ::: List(p))
      }
    }
    if (dependencypaths.size > 0) {
      //      println("before generalization: ")
      //      dependencypaths.foreach(println)
      return (true, generalizeDependencypaths(arg1, arg2, dependencypaths))
    } else {
      return (false, null)
    }
  }

  /** Following findDepPath, with specific dependency-path-length
    */
  def findDepPathWithSpeLen(ns: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode],
    ds: Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]],
    arg1: Int, arg2: Int, pl: Int): (Boolean, List[Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]]) = {

    var deppathlist: List[Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]] = List()

    //pl = 0, special case
    if (pl == 0) return (true, null)

    //pl = 1, just check whether there's a link between "from" and "to"
    if (pl == 1) {
      val checkres = checkpath(ds, arg1, arg2)
      if (checkres._1 == true)
        return (true, List(Set(checkres._2)))
      else return (false, null)
    }

    //pl = 2, only check one other node
    if (pl == 2) {
      val posconnodes = findConnectNodes(ns, List(arg1, arg2), 1)
      posconnodes.foreach {
        case pcn => {
          val checkres_arg1 = checkpath(ds, arg1, pcn.head.id)
          val checkres_arg2 = checkpath(ds, arg2, pcn.head.id)
          if (checkres_arg1._1 == true && checkres_arg2._1 == true) {
            //add result list
            deppathlist = deppathlist ::: List(Set(checkres_arg1._2, checkres_arg2._2))
          }
        }
      }

      if (deppathlist.size > 0)
        return (true, deppathlist)
      else
        return (false, null)
    }

    //pl > 2, general case
    if (pl > 2) {
      val posconnodes = findConnectNodes(ns, List(arg1, arg2), pl - 1) //find all possible sets that could connect arg1 and arg2
      posconnodes.foreach {
        case pcn => { //for each set
          val pcnsize = pcn.size
          for (i <- 0 to pcnsize - 1) {
            val node1 = pcn(i) //for node1
            for (j <- 0 to pcnsize - 1) {
              val node2 = pcn(j)
              if (!node1.id.equals(node2.id)) { //for node2, distinguish from node1
                val checkres_arg1 = checkpath(ds, arg1, node1.id) //is arg1 connected with node1
                val checkres_arg2 = checkpath(ds, arg2, node2.id) //is arg2 connected with node1
                var newns = ns.filter(x => !x.id.equals(arg1) && !x.id.equals(arg2)) //new nodes, removing arg1 and arg2
                var recursivechecker = findDepPathWithSpeLen(newns, ds, node1.id, node2.id, pl - 2) //is node1 connected with node2, this is recursive version
                if (checkres_arg1._1 == true && checkres_arg2._1 == true
                  && recursivechecker._1 == true) { //if arg1<-->node1 && arg2<-->node2 && node1<-->node2
                  var recursivepath = recursivechecker._2 //get the path between node1 and node2
                  recursivepath.foreach {
                    case f => { //add each possible previous path (between node1 and node2) to the new path, adding arg1 and arg2
                      deppathlist = deppathlist ::: List(Set(checkres_arg1._2, checkres_arg2._2) ++ f)
                    }
                  }
                }
              }
            }
          }
        }
      }

      //if results is not empty, return
      if (deppathlist.size > 0)
        return (true, deppathlist)
      else
        return (false, null)
    }

    return (false, null)
  }

  /** find subsets of ns
    * restriction1: the subsets cannot contain any node in "notconsider"
    * restriction2: the size of the subsets == num
    */
  def findConnectNodes(ns: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode],
    notconsider: List[Int], num: Int): List[List[org.allenai.nlpstack.parse.graph.TokenDependencyNode]] = {
    var connectNodeslist: List[List[org.allenai.nlpstack.parse.graph.TokenDependencyNode]] = List()

    var newns: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode] = List()
    ns.foreach(x => if (!notconsider.contains(x.id)) newns = newns ::: List(x))

    subsetswithn(newns, num).foreach {
      case p => {
        //     var l = p.filter(x => (!notconsider.contains(x.string)))
        //        var l:List[org.allenai.nlpstack.core.parse.graph.DependencyNode] = List()
        //        p.foreach(x => if(!notconsider.contains(x.id)) l=l:::List(x))
        //        if(l.size == num){
        //          connectNodeslist = connectNodeslist ::: List(l)
        //        }
        connectNodeslist = connectNodeslist ::: List(p)
      }
    }
    return connectNodeslist
  }

  /** permutation of xs, return all possibilities (without any restriction)
    */
  def subsetswithn(xs: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode], num: Int) = {
    (num to num flatMap (x => xs.combinations(x))) map (x => x)
  }

  //  def subsets3(xs:List[Int], num:Int) = {
  //    (num to num flatMap (x => xs.combinations(x))) map ( x => x)
  //  }

  /** check: if arg1 and arg2 are connected or not?
    */
  def checkpath(ds: Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]],
    arg1: Int, arg2: Int): (Boolean, org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]) = {
    ds.foreach {
      case edge => {
        if ((edge.source.id.equals(arg1) && edge.dest.id.equals(arg2))
          || (edge.source.id.equals(arg2) && edge.dest.id.equals(arg1)))
          return (true, edge)
      }
    }
    return (false, null)
  }

  /** add pos features
    */
  //  def addPOSFeatures(arg1:Int, arg2:Int, 
  //      ori: List[Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]])
  //  		:Set[Set[String]] = {
  //    var generalized:List[Set[]] = List()
  //    return null
  //  }

  /**
    */
  def generalizeDependencypaths(arg1: Int, arg2: Int,
    ori: List[Set[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]]): Set[Set[String]] = {
    //    println("===========================")
    //    println("ori" + ori)
    var generalized: List[Set[String]] = List()
    var init = 3
    var otherargs: Map[Int, String] = collection.mutable.Map.empty[Int, String]

    ori.foreach {
      case eachset => { //for each set
        var eachset_posfealist: List[String] = List()
        var eachset_lexfealist: List[String] = List()
        var eachsetlist = eachset.toList
        var eachset_strver: List[String] = List()
        eachsetlist.foreach {
          case edge => { //for each edge
            var edgestr, source, dest = ""
            var label = edge.label
            if (edge.source.id == arg1 || edge.source.id == arg2) {
              if (edge.source.id == arg1) {
                source = "_1"
                /**eachset_posfealist = addposfea(edge.source, "_1", eachset_posfealist)**/
              } else if (edge.source.id == arg2) {
                source = "_2"
                /**eachset_posfealist = addposfea(edge.source, "_2", eachset_posfealist)**/
              }
              var argname = ""
              if (!otherargs.contains(edge.dest.id)) { argname = "_" + init.toString; otherargs.put(edge.dest.id, argname); init = init + 1 }
              else { argname = otherargs(edge.dest.id) }
              eachset_lexfealist = addlexfea(edge.dest, argname, eachset_lexfealist)
            } else {
              if (!otherargs.contains(edge.source.id)) { source = "_" + init.toString; otherargs.put(edge.source.id, source); init = init + 1; }
              else { source = otherargs(edge.source.id) }
              eachset_posfealist = addposfea(edge.source, source, eachset_posfealist)
            }

            if (edge.dest.id == arg1 || edge.dest.id == arg2) {
              if (edge.dest.id == arg1) {
                dest = "_1";
                /**eachset_posfealist = addposfea(edge.dest, "_1", eachset_posfealist)**/
              } else if (edge.dest.id == arg2) {
                dest = "_2";
                /**eachset_posfealist = addposfea(edge.dest, "_2", eachset_posfealist)**/
              }
              var argname = ""
              if (!otherargs.contains(edge.source.id)) { argname = "_" + init.toString; otherargs.put(edge.source.id, argname); init = init + 1 }
              else { argname = otherargs(edge.source.id) }
              eachset_lexfealist = addlexfea(edge.source, argname, eachset_lexfealist);
            } else {
              if (!otherargs.contains(edge.dest.id)) { dest = "_" + init.toString; otherargs.put(edge.dest.id, dest); init = init + 1 }
              else { dest = otherargs(edge.dest.id) }
              eachset_posfealist = addposfea(edge.dest, dest, eachset_posfealist)
            }

            edgestr = label + "(" + source + ", " + dest + ")"
            eachset_strver = eachset_strver ::: List(edgestr)
          }
        }
        //dependency-path
        generalized = generalized ::: List(eachset_strver.toSet)

        //dependency-path + pos-features
        //     generalized = generalized:::List(eachset_strver.toSet ++ eachset_posfealist.toSet)

        //dependency-path + lex-features
        generalized = generalized ::: List(eachset_strver.toSet ++ eachset_lexfealist.toSet)
      }
    }

    return generalized.toSet
  }

  def addposfea(node: org.allenai.nlpstack.parse.graph.TokenDependencyNode,
    argname: String, posfeaslist: List[String]): List[String] = {
    var posfeaslist_updated: List[String] = List()
    var posfea = "token:pos(" + argname + ", " + node.postag + ")"
    if (!posfeaslist.contains(posfea))
      posfeaslist_updated = posfeaslist ::: List(posfea)
    else
      posfeaslist_updated = posfeaslist
    return posfeaslist_updated
  }

  def addlexfea(node: org.allenai.nlpstack.parse.graph.TokenDependencyNode,
    argname: String, lexfeaslist: List[String]): List[String] = {
    var lexfeaslist_updated: List[String] = List()
    var lexfea = "token:lemma(" + argname + ", " + node.lemma + ")"
    if (!lexfeaslist.contains(lexfea))
      lexfeaslist_updated = lexfeaslist ::: List(lexfea)
    else
      lexfeaslist_updated = lexfeaslist
    return lexfeaslist_updated
  }

  def findHeadW(MYPolyparser: Polyparser, ns: List[org.allenai.nlpstack.parse.graph.TokenDependencyNode], arg1name: String,
    ds: List[org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]]): List[Int] = {
    var res: List[Int] = List()
    val words = arg1name.toString().split(" ").toList
    val wlen = words.length
    for (w1 <- words) {
      val w1ids = MYPolyparser.getid(ns, w1)
      for (w1id <- w1ids) {
        val destnames: List[String] = MYPolyparser.getDestNodes(w1id, ds)
        var flag = true
        for (w2 <- words) {
          if (!w1.equals(w2)) {
            if (!destnames.contains(w2))
              flag = false
          }
        }
        if (flag == true)
          res = res ::: List(w1id)
      }
    }

    return res
  }
}