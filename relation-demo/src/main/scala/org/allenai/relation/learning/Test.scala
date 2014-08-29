package org.allenai.relation.learning

import java.io.File
import org.allenai.relation.util.Polyparser
import scala.collection.mutable.Map
import scala.collection.immutable.SortedMap
import scala.io.Source

object Test {
  def main(args: Array[String]) = {
    //    println("\nTest1: file.separator")
    //    val configArffTrain = "arff" + File.separator + "train.arff"
    //    println(configArffTrain)
    //    
//    println("\nTest2: list.map")
//    def compfea1(s: String): List[Integer] = { return List(s.length(), s.length() - 10) }
//    def compfea2(s: String): List[String] = { return List(s) }
//    val list = List("monday", "tuesday", "wednesday")
//    println(list.map { p => (p, compfea1(p)) }.toMap) //list.map{...}.toMap
//    var map1: Map[String, List[Integer]] = collection.mutable.Map.empty[String, List[Integer]]
//    var map2: Map[String, List[String]] = collection.mutable.Map.empty[String, List[String]]
//    list.foreach {
//      case p => {
//        map1.put(p, compfea1(p))
//        map2.put(p, compfea2(p))
//      }
//    }
//    println("map1 = " + map1)
//    println("map2 = " + map2)
//    println(map1.apply("tuesday"))
//    println(map1.apply("tuesday").size)
    //    
    //    println("\nTest3: caseclass")    
    //    SentenceDisrel.fromTrainingFile("/Users/qingqingcai/Documents/Aristo/extraction-new/data/weka/sentence-disrel-labeled.txt", 1).foreach(println)

    //    println("\nTest4: features")
    //    var features = Seq[Double]()
    //    println(features)
    //    features :+= 0.99
    //    println(features)
    //    features :+= 0.12
    //    println(features)

    //    println("\nTest5: split")
    //    val sentence = "this information in intended as a tool to not only provide more concrete information as a tool before ."
    //    val tokens = sentence.split("\\s+").toList
    //    println("tokens: = " + tokens)
    //    val arg1 = "a tool"
    //    val arg1tokens = arg1.split("\\s+").toList
    //    
    //    println("\nTest6: list.flatten")
    //    val list:List[String] = List("an example of", "called as", "a way to", "include", "such as")
    //    var newlist:List[String] = List()
    //    list.foreach(p => p.split("\\s+").foreach(q => newlist=newlist:::List(q)))
    //    println(newlist)

    //    import FeatureWrapper._
    //    import org.allenai.ari.solvers.utils.Tokenizer
    //    println("\nTest7: set.intersect")
    //    val sentence = "this information in intended as a tool to not only provide more concrete information as a tool before ."
    //    val arg1 = "provide more not only information hahahahah"
    //    val sentenceSet = Tokenizer.toKeywords(sentence).toSet
    //    val arg1Set = Tokenizer.toKeywords(arg1).toSet
    //    println(sentenceSet)
    //    println(sentenceSet.intersect(arg1Set))
    //    println(arg1Set)
    //    println(arg1Set.mkString(" "))

    //    println("\nTest8: entailment")
    //    import org.allenai.ari.solvers.inference.matching.{ EntailmentWrapper, EntailmentService }
    //    val wordnetEntailmentService: EntailmentService = {
    //      val wordnetEntailmentUrl = "http://entailment.dev.allenai.org:8191/api/entails"
    //      val wrapper = new EntailmentWrapper(wordnetEntailmentUrl)
    //      wrapper.CachedEntails
    //    }
    //
    //    def wordnetEntailment(text: String, hypothesis: String) =
    //      wordnetEntailmentService(text, hypothesis) map { _.confidence } getOrElse 0d
    //
    //    val word2vecEntailmentService: EntailmentService = {
    //      val word2vecEntailmentUrl = "???"
    //      val wrapper = new EntailmentWrapper(word2vecEntailmentUrl)
    //      wrapper.CachedEntails
    //    }
    //      
    //    println(wordnetEntailment("is considered to be one of ", "is thought as"))

    //    println("\n9 Convert map to list")
    //    val map1 = Map("1"->Seq("a", "b", "c"), "2"->Seq("d", "b", "a"))
    //    val map2 = Map("3"->Seq("go", "don't", "prefere"))
    //    var nominalmaps = collection.mutable.Map.empty[String, String]
    //    val featuresize = map1.values.toList(0).size
    //    for(i <- 0 to featuresize-1) {
    //      var nominals:Set[String] = Set()
    //      map1.values.foreach(p => nominals += p(i))
    //      map2.values.foreach(p => nominals += p(i))
    //      println(nominals)
    //      nominalmaps.put(i.toString, nominals.mkString(","))
    //    }
    //    println("finally")
    //    nominalmaps.foreach(println)

    //    println("\10 Scala map sorting")
    //    var map = collection.mutable.Map.empty[String, String]
    //    map.put("ABV", "abc")
    //    map.put("BAB", "dog")
    //    map.put("CDQ", "cat")
    //    println(map)
    //    println(SortedMap(map.toSeq:_*))

    //    println("\11 Argument Indexing")
    //    val sentence = "sunlight is the main source of energy for the water cycle."
    //    val arg1name = "sunlight"
    //    val arg2name = "the water cycle"
    //    val (root, tree) = Polyparser.processText(sentence)
    //    tree.vertices.toList.foreach {
    //      p => println(p.id + "\t" + p.string + "\t" + p.postag)
    //    }
    //    println("--------------------------------------------")
    //    tree.edges.toList.foreach {
    //      p => println(p.label)
    //    }
    //
    //    println("root = " + root.id + "\t" + root.string)
    //    val arg1list = Polyparser.findHeadW(tree.vertices.toList, arg1name, tree.edges.toList)
    //    println(arg1list)
    //    val arg2list = Polyparser.findHeadW(tree.vertices.toList, arg2name, tree.edges.toList)
    //    println(arg2list)

    //    var myset: Set[Int] = Set(1,2,4,3,5,1,3,2)
    //    var sorttedmyset = collection.immutable.SortedSet[Int]() ++ myset
    //    println(myset)
    //    println(myset.tail)
    //    println(myset.slice(0, 2))
    //    println(myset.toList(0))
    //    println()
    //    println(sorttedmyset)
    //    println(sorttedmyset.tail)
    //    println(sorttedmyset.slice(0, 2))
    //    println(sorttedmyset.toList(0))

    //    var res: List[Int] = List()
    //
    //    var length = 2
    //    val myset = Set(1,2,4,5,6,7,9)
    //    val issortedset = collection.immutable.SortedSet[Int]() ++ myset
    //    println("set = " + issortedset)
    //    var subset: Set[Int] = Set()
    //    var i = 0
    //    while(i<issortedset.size && i+length<=issortedset.size) {
    //      val subset = issortedset.slice(i, i+length)
    //      println("length = " + length + "\t\t\tsubset = " + subset)
    //      var j = 1
    //      while (j<subset.size) {
    //        println("\tfirst = " + subset.slice(j-1, j).toList(0))
    //        println("\tsecond = " + subset.slice(j, j+1).toList(0))
    //    	if(subset.slice(j-1, j).toList(0) == subset.slice(j, j+1).toList(0)-1) j = j+1
    //    	else j = Integer.MAX_VALUE
    //      }
    //      println("j = " + j)
    //      if(j==subset.size) res = res ::: List(subset.toList(0))
    //      i = i+1
    //    }
    //    
    //    println(res)
    
//    val str = "str1	str2"
//    println(str.indexOf("\t"))
//    println(str.substring(0, str.indexOf("\t")))
//    println(str.substring(str.indexOf("\t")+1, str.length()))
    
    /**
   * Convert sentences to testing-instances
   */
    val file = "data/binary/inputDirectory/108Q.txt"
  	Source.fromFile(file).getLines().map {
      line => 
        
    }.toList
  }
}