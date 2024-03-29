package org.allenai.relation.learning

import scala.collection.mutable.Map
import org.allenai.relation.learning._
import org.allenai.relation.util.Polyparser
import scala.io.Source
import org.allenai.common.Logging
import java.io.PrintWriter

object ArgumentGuessing extends App with Logging {
  /** Global variables
    */
  var disrelFeaturesMap: Map[String, Set[Set[Path]]] = collection.mutable.Map.empty
  run()

  System.exit(0)

  // ****************************************************************************************

//  def test() = {
//    val sentence = "humans depend on water from the environment."
//    init("data/binary-argument/Logistic/feature/train-lexical-detail-length-new.feature")
//    printDisrelFeatures()
//    val disrelArgsSet = genArgCandidates(sentence)
//    println(sentence)
//    disrelArgsSet.foreach(p => println(p._1 + "\t" + p._2._1 + "\t" + p._2._2))
//  }

  def run() = {
    init("data/learning/classifier/binary-argument/Logistic/feature/train-lexical-detail-length-new.feature")
    printDisrelFeatures()
    val configSentence = "data/binary-argument/inputDirectory/108Q.txt"
    val configTestInstance = "data/binary-argument/inputDirectory/108Q_Arg.txt"
    toTestTxt(configSentence, configTestInstance)
  }

  /** Given a sentence, identify all argument-pair candidates that satisfy the dependency features
    * Input: String = sentence
    * 		eg: water and food are required for animals to get growth.
    * Output: List[(String, String)] = all-(arg1, arg2)-candidates
    * 		eg: List(("water and food", "animals"), ("water", "get growth"), ....)
    */
  def genArgCandidates(sentence: String): Set[(String, (String, String))] = {
    var disrelArgsSet: Set[(String, (String, String))] = Set()
    val (root, tree) = Polyparser.processText(sentence)
    if (tree == null)
      return null
    var target = "_1"
    disrelFeaturesMap.foreach {
      case (disrel, featuresSet) => {
        featuresSet.foreach {
          case feature =>
            var argpair = findArgPair(tree, feature)
            if (argpair != null)
              argpair.foreach(arg => disrelArgsSet = disrelArgsSet + ((disrel, arg)))
        }
      }
    }
    return disrelArgsSet
  }

  /** Getting all possible argument candidates which satisfy the feature (dependency path) constraints
    * Input: tree, feature =
    * Output: Set((changes,matter), (melts, cube), (melts, ice))
    */
  def findArgPair(tree: Polyparser.Mygraph, feature: Set[Path]): Set[(String, String)] = {
    val edges = tree.edges
    var connarg: String = null
    var connargname: String = null
    var arg1: String = null
    var arg2: String = null
    var argpairs: Set[(String, String)] = Set()
    // for _1
    var target = "_1"
    var path: Path = findPathWithSpeargWithoutSpearg(feature, target, null) // path with _1
    if (path != null) {
      val edgesetwithspelabel = Polyparser.findEdgeWithName(edges, path.label) // edge where edge.label = path.label
      if (edgesetwithspelabel.size == 0) return null // if there are no edges, whose label == path.edge, not satisfying the feature constraint
      if (path.source.equals(target)) { // path.source == _1
        connarg = path.dest // connecting word is path.dest	
        edgesetwithspelabel.foreach {
          case edge => { // for each edge where edge.label = path.label
            arg1 = edge.source.string // arg1 candidate
            connargname = edge.dest.string
            // find arg2 candidates
            val arg2set = findArg2Candidate(tree, feature, connarg, connargname, target)
            if (arg2set != null && arg2set.size > 0)
              arg2set.foreach(arg2 => argpairs = argpairs + ((arg1, arg2)))
          }
        }
      } else if (path.dest.equals(target)) { // path.dest == _1
        connarg = path.source // connecting word is path.source
        edgesetwithspelabel.foreach {
          case edge => { // for each edge where edge.label = path.label
            arg1 = edge.dest.string // arg1 candidate
            connargname = edge.source.string
            // find ar2 candidates
            val arg2set = findArg2Candidate(tree, feature, connarg, connargname, target)
            if (arg2set != null && arg2set.size > 0)
              arg2set.foreach(arg2 => argpairs = argpairs + ((arg1, arg2)))
          }
        }
      }
    }

    return argpairs
  }

  /** Find arg2 candidate
    */
  def findArg2Candidate(tree: Polyparser.Mygraph, feature: Set[Path],
    target: String, connargname: String, pretarget: String): Set[String] = {
    var arg2set: Set[String] = Set()
    val edges = tree.edges
    if (target.equals("_2")) {
      val path = findPathWithSpeargWithoutSpearg(feature, target, null)
      if (path != null) {
        val edgesetwithspelabel = Polyparser.findEdgeWithName(edges, path.label)
        if (edgesetwithspelabel.size > 0) {
          if (path.source.equals(target)) {
            edgesetwithspelabel.foreach {
              case edge => {
                if (connargname.equals(edge.source.string))
                  arg2set = arg2set + edge.source.string
              }
            }
          } else if (path.dest.equals(target)) {
            edgesetwithspelabel.foreach {
              case edge => {
                if (connargname.equals(edge.dest.string))
                  arg2set = arg2set + edge.dest.string
              }
            }
          }
        }
      }
    } else {
      val path = findPathWithSpeargWithoutSpearg(feature, target, pretarget)
      if (path != null) {
        val edgesetwithspelabel = Polyparser.findEdgeWithName(edges, path.label)
        if (edgesetwithspelabel.size > 0) {
          if (path.source.equals(target)) {
            edgesetwithspelabel.foreach {
              case edge => {
                val connarg = path.dest
                if (connargname.equals(edge.source.string))
                  return findArg2Candidate(tree, feature, connarg, edge.dest.string, target)
              }
            }
          } else if (path.dest.equals(target)) {
            edgesetwithspelabel.foreach {
              case edge => {
                val connarg = path.source
                if (connargname.equals(edge.dest.string))
                  return findArg2Candidate(tree, feature, connarg, edge.source.string, target)
              }
            }
          }
        }
      }
    }
    return arg2set
  }

  /** Convert sentences to testing-instances
    */
  def toTestTxt(configsentence: String, configtestinstance: String) = {
    //sid	sentence	disrel	relphrase	arg1	arg2	annotationOpt
    val writer = new PrintWriter(configtestinstance, "utf-8")
    writer.println("sid" + "\t" + "sentence" + "\t" + "disrel" + "\t" + "relphrase" + "\t" + "arg1" + "\t" + "arg2" + "\t" + "annotationOpt")
    var id = 1
    Source.fromFile(configsentence).getLines().foreach({
      line =>
        val disrelArgsSet = genArgCandidates(line)
        disrelArgsSet.foreach(p => { // p = (disrel, argpair)
          println(p._1 + "\t" + p._2._1 + p._2._2)
          val instance = id.toString + "\t" + line + "\t" + p._1 + "\t" + "" + "\t" + p._2._1 + "\t" + p._2._2 + "\t" + "0"
          writer.println(instance)
          id = id + 1
        })
    })
    writer.close()
  }

  /** Initialize global variables
    * Input line: nominal-spec-deplength3=purpose => (nsubj(_3, _1), dobj(_4, _2), xcomp(_3, _4))	59.55700355021503
    * Output: map.key = purpose; map.value = Set(purpose => (nsubj(_3, _1), dobj(_4, _2), xcomp(_3, _4)), ......)
    */
  def init(file: String) = {
    Source.fromFile(file).getLines().foreach {
      case line =>
        val index1 = line.indexOf("=")
        val index2 = line.indexOf("\t")
        val featurestr = line.substring(index1 + 1, index2)
        val confidencestr = line.substring(index2 + 1)
        val confidence = confidencestr.toDouble
        if (confidence > 0) {
          val split = " => "
          val (disrel, feature) = Converting.parseFeature(featurestr, split)
          if (disrel != null && feature != null) {
            if (!disrelFeaturesMap.contains(disrel)) disrelFeaturesMap.put(disrel, Set(feature))
            else disrelFeaturesMap.put(disrel, disrelFeaturesMap(disrel) + feature)
          }
        }
    }
  }

  /** In a set of paths, find the path with specific-arg-name = argname (it could be the source or the dest)
    */
  def findPathWithSpeargWithoutSpearg(pathsets: Set[Path], witharg: String, withoutarg: String): Path = {
    pathsets.foreach {
      case path =>
        if (path.dest.equals(witharg) || path.source.equals(witharg))
          if (withoutarg == null) return path
          else if (!path.dest.equals(withoutarg) && !path.source.equals(withoutarg)) return path
    }
    return null
  }

  /** Input: disrel -> set-of-features; each feature is represented by set-of-path
    */
  def printDisrelFeatures() = {
    logger.info(s"selected dependency patterns")
    disrelFeaturesMap.foreach {
      case (disrel, feature) => {
        println("disrel = " + disrel + "\t" + "size = " + feature.size)
        feature.foreach { p => p.foreach(f => print(f.tostring + "\t")); println() }
      }
    }
  }
}

/** Algorithm for getting argument candidates
  * // target = _1
  * // find the path.label where "path.source == target or path.dest == target"
  * // if path.source == target
  * // get connarg = path.dest, get edge where "edge.dest == path.dest", arg1 = edge.source, connargname = edge.dest
  * // else if path.dest == target
  * // get connarg = path.source, get edge where "edge.label == path.label", arg1 = edge.source, connargname = edge.source
  * // target = connarg
  *
  * // while(target != _2)
  * // find the path.label where "path.source == target or path.dest == target"
  * // if path.source == target
  * // get connarg = path.dest, get edge where "edge.label == path.label", if(connargname != edge.source) return false; else connargname = edge.dest
  * // else if path.dest == target
  * // get connarg = path.source, get edge where "edge.label == path.label", if(connargname != edge.dest) return false; else connargname = edge.source
  * // target = connarg
  *
  * // if(target == _2)
  * // if path.source == target
  * // get edge where "edge.label == path.label", arg2 = edge.source
  * // else if path.dest == target
  * // get edge where "edge.label == path.label", arg2 = edge.dest
  */

//    // for testing, just initialize the disrelFeaturesMap = ...
//    disrelFeaturesMap.put("purpose", Set(Converting.parseFeature("purpose => (nn(_3, _1), conj(_3, _2))", " => ")._2, 
//        Converting.parseFeature("purpose => (pobj(_3, _1), nsubj(_4, _2), prep(_4, _3))", " => ")._2, 
//        Converting.parseFeature("purpose => (appos(_1, _3), pobj(_4, _2), prep(_3, _4))", " => ")._2, 
//        Converting.parseFeature("purpose => (nn(_1, _2))", " => ")._2))
//    disrelFeaturesMap.put("example", Set(Converting.parseFeature("example => (vmod(_1, _3), det(_4, _2), dobj(_3, _4))", " => ")._2, 
//        Converting.parseFeature("example => (nsubjpass(_3, _1), xcomp(_3, _2))", " => ")._2, 
//        Converting.parseFeature("purpose => (pobj(_3, _1), nsubj(_4, _2), prep(_4, _3))", " => ")._2))