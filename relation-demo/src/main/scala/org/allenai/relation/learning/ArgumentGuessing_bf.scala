package org.allenai.relation.learning

import scala.collection.mutable.Map
import org.allenai.relation.learning._
import org.allenai.relation.util.Polyparser

object ArgumentGuessing_bf extends App {
  /** Global variables
    */
  var disrelFeaturesMap: Map[String, Set[Set[Path]]] = collection.mutable.Map.empty

  //  var sentence = "temperature and sky conditions are both used to describe weather."
  //  var sentence = "humans depend on water from the environment."
  var sentence = "when an ice cube melts, its state of matter changes from a solid to a liquid."
  init("")
  genArgCandidates(sentence)
  System.exit(0)

  // ****************************************************************************************

  /**
    */
  def run(sentence: String) = {

  }

  /** Given a sentence, identify all argument-pair candidates that satisfy the dependency features
    * Input: String = sentence
    * 		eg: water and food are required for animals to get growth.
    * Output: List[(String, String)] = all-(arg1, arg2)-candidates
    * 		eg: List(("water and food", "animals"), ("water", "get growth"), ....)
    */
  def genArgCandidates(sentence: String) = {
    val (root, tree) = Polyparser.processText(sentence)
    val verts = tree.vertices
    val edges = tree.edges

    println("parsing treeL ")
    edges.foreach(p => println(p.label + "\t" + p.source.string + "\t" + p.dest.string))

    var target = "_1"
    disrelFeaturesMap.foreach {
      case (disrel, featuresSet) => {
        featuresSet.foreach {
          case feature =>
            println()
            println("feature: ")
            feature.foreach(p => print(p.tostring + "\t"))
            println()

            var argpair = findArgPair(tree, feature)
            if (argpair != null) argpair.foreach(p => println("path = " + p))
            else println("argpair is null!")
        }
      }
    }
  }

  /**
    */
  // target = _1
  // find the path.label where "path.source == target or path.dest == target"
  // if path.source == target
  // get connarg = path.dest, get edge where "edge.dest == path.dest", arg1 = edge.source, connargname = edge.dest
  // else if path.dest == target
  // get connarg = path.source, get edge where "edge.label == path.label", arg1 = edge.source, connargname = edge.source
  // target = connarg

  // while(target != _2)
  // find the path.label where "path.source == target or path.dest == target"
  // if path.source == target
  // get connarg = path.dest, get edge where "edge.label == path.label", if(connargname != edge.source) return false; else connargname = edge.dest
  // else if path.dest == target
  // get connarg = path.source, get edge where "edge.label == path.label", if(connargname != edge.dest) return false; else connargname = edge.source
  // target = connarg

  // if(target == _2)
  // if path.source == target
  // get edge where "edge.label == path.label", arg2 = edge.source
  // else if path.dest == target
  // get edge where "edge.label == path.label", arg2 = edge.dest
  def findArgPair(tree: Polyparser.Mygraph, feature: Set[Path]): Set[(String, String)] = {
    val edges = tree.edges
    var connarg: String = null
    var connargname: String = null
    var arg1: String = null
    var arg2: String = null
    var argpairs: Set[(String, String)] = Set()
    // for _1
    var target = "_1"
    var path: Path = findPathWithSpeargWithoutSpearg(feature, target, null)
    if (path != null) {
      println("path with arg=_1: " + path.tostring)
      val edgesetwithspelabel = Polyparser.findEdgeWithName(edges, path.label)
      println("edgesetwithspeclabel = " + path.label + ", size = " + edgesetwithspelabel)

      if (edgesetwithspelabel.size == 0)
        return null
      if (path.source.equals(target)) {
        connarg = path.dest
        edgesetwithspelabel.foreach {
          case edge => { // for each edge where edge.label = path.label
            println("processing the edge for arg1 in source: " + edge.toString)
            arg1 = edge.source.string // arg1 candidate
            connargname = edge.dest.string
            val arg2set = helper(tree, feature, connarg, connargname, target)
            if (arg2set != null && arg2set.size > 0)
              arg2set.foreach(arg2 => argpairs = argpairs + ((arg1, arg2)))
          }
        }
      } else if (path.dest.equals(target)) {
        connarg = path.source
        edgesetwithspelabel.foreach {
          case edge => {
            println("processing the edge for arg1 in dest: " + edge.toString)
            arg1 = edge.dest.string
            connargname = edge.source.string
            val arg2set = helper(tree, feature, connarg, connargname, target)
            if (arg2set != null && arg2set.size > 0)
              arg2set.foreach(arg2 => argpairs = argpairs + ((arg1, arg2)))
          }
        }
      }
    }

    return argpairs
  }

  def helper(tree: Polyparser.Mygraph, feature: Set[Path],
    target: String, connargname: String, pretarget: String): Set[String] = {

    println("helper(): " + target + " &&& " + connargname)
    val edges = tree.edges

    var arg2set: Set[String] = Set()
    if (target.equals("_2")) {
      val path = findPathWithSpeargWithoutSpearg(feature, target, null)
      val edgesetwithspelabel = Polyparser.findEdgeWithName(edges, path.label)
      if (path != null && edgesetwithspelabel.size > 0) {
        if (path.source.equals(target)) {
          edgesetwithspelabel.foreach {
            case edge => {
              println("processing the edge for arg2 in source: " + edge.toString)
              if (connargname.equals(edge.source.string))
                arg2set = arg2set + edge.source.string
            }
          }
        } else if (path.dest.equals(target)) {
          edgesetwithspelabel.foreach {
            case edge => {
              println("processing the edge for arg2 in dest: " + edge.toString)
              if (connargname.equals(edge.dest.string))
                arg2set = arg2set + edge.dest.string
            }
          }
        }
      }

    } else {
      val path = findPathWithSpeargWithoutSpearg(feature, target, pretarget)
      println("else path = " + path.tostring)
      val edgesetwithspelabel = Polyparser.findEdgeWithName(edges, path.label)

      if (path != null && edgesetwithspelabel.size > 0) {
        if (path.source.equals(target)) {
          edgesetwithspelabel.foreach {
            case edge => {
              println("processing the edge for conn in source: " + edge.toString)
              val connarg = path.dest
              if (connargname.equals(edge.source.string))
                return helper(tree, feature, connarg, edge.dest.string, target)
            }
          }
        } else if (path.dest.equals(target)) {
          edgesetwithspelabel.foreach {
            case edge => {
              println("processing the edge for conn in dest: " + edge.toString)
              val connarg = path.source
              if (connargname.equals(edge.dest.string))
                return helper(tree, feature, connarg, edge.source.string, target)
            }
          }
        }
      }
    }
    println("getting here")
    arg2set.foreach(p => print("----" + p))
    println()
    return arg2set
  }

  /** Initialize global variables
    */
  def init(featurefilename: String) = {
    // for testing, just initialize the disrelFeaturesMap = ...
    disrelFeaturesMap.put("purpose", Set(Converting.parseFeature("purpose => (nn(_3, _1), conj(_3, _2))", " => ")._2,
      Converting.parseFeature("purpose => (pobj(_3, _1), nsubj(_4, _2), prep(_4, _3))", " => ")._2,
      Converting.parseFeature("purpose => (appos(_1, _3), pobj(_4, _2), prep(_3, _4))", " => ")._2,
      Converting.parseFeature("purpose => (nn(_1, _2))", " => ")._2))
    //    disrelFeaturesMap.put("example", Set(Converting.parseFeature("example => (vmod(_1, _3), det(_4, _2), dobj(_3, _4))", " => ")._2, 
    //        Converting.parseFeature("example => (nsubjpass(_3, _1), xcomp(_3, _2))", " => ")._2, 
    //        Converting.parseFeature("purpose => (pobj(_3, _1), nsubj(_4, _2), prep(_4, _3))", " => ")._2))
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
}