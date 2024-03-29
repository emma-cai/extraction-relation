package org.allenai.relation.test
import org.allenai.relation.api._
import org.allenai.relation.util._
import org.allenai.extraction.{ ErmineException, FlatProcessor }
import org.allenai.extraction.rdf.{ DependencyGraph, Token }
import org.allenai.extraction.rdf.DependencyGraph.GraphRdf
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.parse.PolytreeParser
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer
import com.tinkerpop.blueprints.impls.sail.impls.MemoryStoreSailGraph

object APITest {
  def main(args: Array[String]) = {
    //    val MYAPI = new SearchInstance()
    //    println(MYAPI.insSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", "CAUSE", "because of"))

    //    val MYSearch: Searching = new Searching()
    //    val seed = "because"
    //    val perseed = MYSearch.runSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", "\"" + seed + "\"", "kp", List("arg1", "arg2"), 1000)
    //    perseed.foreach(println)
    //    println(perseed.size)

    //    val MYAPI = new SentenceSearching()
    //    println(MYAPI.senSearch("/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_v2", "CAUSE", "gas", "bacteria"))

    //    val MYAPI = new DependencySearching()
    //    val res = MYAPI.runSearch("/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_dp", "CAUSE")
    //    res.foreach(println)

    //    val sen: String = "Your gifts offer children a healthy , successful future - which creates a stronger community for all of us ."
    //    val (root, tree) = Polyparser.processText(sen)
    //    println("root = " + root.string)
    //    println("tree = " + tree.toString)

    //    tree.vertices.foreach(p => println(p.string))
    //    tree.edges.foreach(p => println(p.toString))
    //    val rootid = MYPolyparser.getid(tree.vertices.toList, "root")
    //    println(rootid)
    //    val source = scala.io.Source.fromFile("data/binary/inputDirectory/barrons.txt")
    //    val corpus = Token.corpus(source)
    //    println(corpus)

    val list1 = List("a", "b", "c", "d")
    val list2 = List(1, 3, 2, 0)
    val tmp = list1.zip(list2)
    println(list1)
    println(list2)
    println(tmp)
    val sorted = tmp.sortWith(_._2 > _._2)
    println(sorted)
    var list1sorted: List[String] = List()
    var list2sorted: List[Int] = List()
    sorted.foreach(p => {
      list1sorted = list1sorted ::: List(p._1);
      list2sorted = list2sorted ::: List(p._2)
    })
    println(list1sorted)
    println(list2sorted)
  }
}