package org.allenai.relation.learning

import java.io.{ InputStream, File }
import scala.io.Source
import org.allenai.common.Resource
import org.allenai.ari.solvers.inference.matching.{ EntailmentWrapper, EntailmentService }
import org.allenai.ari.solvers.utils.Tokenizer
import org.allenai.relation.util.Polyparser

object FeatureWrapper {
  
  
  private def getResourceAsStream(name: String): InputStream =
    getClass.getClassLoader.getResourceAsStream(name)

  //  val wordFrequency = loadWordFrequencies("word-frequencies.txt")
  //  val minFreq = wordFrequency.values.min

  def overlap(text: Set[String], hypothesis: Set[String]) =
    text.intersect(hypothesis).size

  import Tokenizer._
  //  def hypothesisCoverage(text: String, hypothesis: String): Double = {
  //    hypothesisCoverage(toKeywords(text).toSet, toKeywords(hypothesis).toSet)
  //  }

  def distance(sentence: String, arg1: String, arg2: String): Double = {
    val tokens = sentence.split("\\s+").toList
    val arg1s = arg1.split("\\s+").toList
    val arg2s = arg2.split("\\s+").toList
    val arg1sIndex = tokens.indexOfSlice(arg1s)
    val arg2sIndex = tokens.indexOfSlice(arg2s)
    // problems: (1) ...+length(arg1 or arg2); (2) arg1 appears multiple times
    Math.abs(arg1sIndex - arg2sIndex).toDouble
  }
  //  Math.abs(sentence.indexOf(arg1) - sentence.indexOf(arg2)).toDouble

  //  private def frequencyWeight(token: String): Double = {
  //    // constants were hand-tuned by Peter
  //    val wordWeightK: Double = 10.0
  //    val normalizationConstant: Double = 2.3978953
  //    (1 / math.log(wordFrequency.getOrElse(token, minFreq) + wordWeightK)) * normalizationConstant
  //  }
  //
  //  /** Load word frequency information from a file of frequency<space>term pairs.
  //    */
  //  private def loadWordFrequencies(path: String): Map[String, Int] = {
  //    val wordFrequenciesStream = getResourceAsStream(path)
  //    val counts = scala.collection.mutable.HashMap[String, Int]()
  //    Resource.using(Source.fromInputStream(wordFrequenciesStream)) {
  //      input =>
  //        for (line <- input.getLines()) {
  //          line.split("\\s+") match {
  //            case Array(count, term) => {
  //              counts.update(term, count.toInt + counts.getOrElse(term, 0))
  //            }
  //            case _ => throw new MatchError("Couldn't parse line " + line + " from " + path)
  //          }
  //        }
  //    }
  //    counts.toMap
  //  }
  //
  //  def tfIdf(text: Set[String], hypothesis: Set[String]) = {
  //    val hypWeights = hypothesis.map { frequencyWeight(_) }
  //    val overlapWeights = text.intersect(hypothesis) map { frequencyWeight(_) }
  //    overlapWeights.sum / hypWeights.sum
  //  }
  //
  val wordnetEntailmentService: EntailmentService = {
    val wordnetEntailmentUrl = "http://entailment.dev.allenai.org:8191/api/entails"
    val wrapper = new EntailmentWrapper(wordnetEntailmentUrl)
    wrapper.CachedEntails
  }

  def wordnetEntailment(text: String, hypothesis: String) =
    wordnetEntailmentService(text, hypothesis) map { _.confidence } getOrElse 0d

  val word2vecEntailmentService: EntailmentService = {
    val word2vecEntailmentUrl = "???"
    val wrapper = new EntailmentWrapper(word2vecEntailmentUrl)
    wrapper.CachedEntails
  }
    
  def getrootstring(root: Polyparser.Mytokennode): String = {
    var rootstring = "null"
    if(root != null) rootstring = root.string	//cannot contain any " " or some special characters
    rootstring
  }
  
  
  def getshortestpath(root: Polyparser.Mytokennode, tree: Polyparser.Mygraph, 
	  arg1list: List[Int], arg2list: List[Int], pathlength:Int):String = {
    //org.allenai.nlpstack.graph.Graph.Edge[org.allenai.nlpstack.parse.graph.TokenDependencyNode]

    arg1list.foreach(arg1 => arg2list.foreach(arg2 => {
        val (flag, pathsets) = Polyparser.findDepPathWithSpeLen(tree.vertices.toList, tree.edges, arg1, arg2, pathlength)
        if(flag == true) {
          return "\""+Polyparser.generalizeDependencypaths(arg1, arg2, pathsets.toList)(0).mkString(", ")+"\""
        }
      //    return pathsets.toList(0).toString
          
    }))
    return "null"
  }
  //
  //  //GregJ
  //  def word2vecEntailment(text: String, hypothesis: String) = ???
  //
  //  //Ellie
  //  def ppdbEntailment(text: String, hypothesis: String) = ???

}
