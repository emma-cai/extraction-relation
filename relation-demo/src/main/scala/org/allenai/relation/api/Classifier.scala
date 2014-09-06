package org.allenai.relation.api

//import org.allenai.relation.learning.BinaryClassification_bf
import org.allenai.relation.learning.BinaryClassification
import java.io.File
import weka.core.Instance
import weka.core.converters.ArffLoader
import weka.core.Instances
import weka.core.FastVector
import java.util.ArrayList
import weka.core.Attribute
import org.allenai.relation.util.Polyparser
import org.allenai.relation.learning.BinarySentenceDisrel
import org.allenai.relation.learning.FeatureWrapper._
import weka.classifiers.Classifier
import weka.core.DenseInstance
import scala.collection.mutable.Map
import org.allenai.relation.learning.ArgumentGuessing

object Classifier {
  private val disrelseeds = collection.mutable.Map(
    ("purpose", List("purpose", "used to", "responsible")),
    ("cause", List("caused", "so that", "because", "result in", "effect on")),
    ("effect", List("caused", "so that", "because", "result in", "effect on")),
    /**("function", List("used to")),**/
    ("example", List("an example of", "called", "a way to", "include", "such as")),
    ("enable", List("to help", "by")),
    ("part", List("part of")),
    ("requirement", List("necessary", "needed")),
    ("condition", List("when", "if")))
  val disrellist = List("purpose", "cause", "example", "enable", "requirement")
  // the only variable which need to be changed when running a classifier
  val configclassifiername = "Logistic"
  val configfeature = "lexical-prep-detail-length-new"
  val directory = "data/binary"

  // ********************************************************************************************************

  def run(sentence: String, arg1: String, arg2: String) = {
    BinaryClassification.init(directory, configclassifiername, configfeature,
      "", "", false, false, disrelseeds, disrellist)

    var predictlist: List[String] = List()
    var confidencelist: List[Double] = List()
    disrellist.foreach {
      case p =>
        val (predict, confidence) = testInstance(sentence, p, arg1, arg2, "1")
        predictlist = predictlist ::: List(p)
        confidencelist = confidencelist ::: List(confidence)
    }
    val pair = predictlist.zip(confidencelist)
    val pairsorted = pair.sortWith(_._2 > _._2)
    var predictlistsorted: List[String] = List()
    var confidencelistsorted: List[String] = List()
    pairsorted.foreach(p => {
      predictlistsorted = predictlistsorted ::: List(p._1);
      confidencelistsorted = confidencelistsorted ::: List(p._2.toString)
    })

    (predictlistsorted, confidencelistsorted)
  }

  //  /**
  //   * TODO........
  //   */
  //  def run_sen(sentence: String) = {
  //    BinaryClassification.init(disrelSeeds, disrelList, configClassifierName,
  //      configFeature, configClassifierModel, configEvalModel, configArffTrain, 
  //      numericfeaturesHeader, nominalfeaturesHeader, isPrintFeatureWeight)
  //      
  //    var predictlist: List[String] = List()
  //    var confidencelist: List[Double] = List()
  //    
  //    val disrelArgsSet = ArgumentGuessing.genArgCandidates(sentence)
  //    
  //    disrelArgsSet.foreach {
  //      case p =>
  //        val (predict, confidence) = testInstance(sentence, p._1, p._2._1, p._2._2, "")
  //        predictlist = predictlist ::: List(p._1)
  //        confidencelist = confidencelist ::: List(confidence)
  //    }
  //    val pair = predictlist.zip(confidencelist)
  //    val pairsorted = pair.sortWith(_._2 > _._2)
  //    var predictlistsorted:List[String] = List()
  //    var confidencelistsorted:List[String] = List()
  //    pairsorted.foreach(p => {predictlistsorted = predictlistsorted:::List(p._1); 
  //    		confidencelistsorted = confidencelistsorted:::List(p._2.toString)})
  //    
  //    (predictlistsorted, confidencelistsorted)
  //  }

  def testInstance(sentence: String, disrel: String, arg1: String, arg2: String, label: String) = {
    // load trained model
    val (classifier, eval) = BinaryClassification.loadModel()

    // extract testing sentences and features
    val sentenceDisrel = BinarySentenceDisrel.fromSingleInstance(sentence, disrel, arg1, arg2, label)
    val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
    val arg1list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg1, tree.edges.toList)
    val arg2list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg2, tree.edges.toList)
    val lengthDependenciesMap = getLengthDependencies(root, tree, arg1list, arg2list)

    val numericfeaturemap = BinaryClassification.getNumericfeatures(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list)
    val nominalfeaturemap = BinaryClassification.getNominalfeatures(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list)
    val doubleFeatures: Seq[Double] = BinaryClassification.getFeatureValueByName(BinaryClassification.numericfeaturesHeader, numericfeaturemap)
    val stringFeatures: Seq[String] = BinaryClassification.getFeatureValueByName(BinaryClassification.nominalfeaturesHeader, nominalfeaturemap)
    val (predict, confidence) = runClassifier(classifier, doubleFeatures, stringFeatures, sentenceDisrel.annotationOpt.toList(0))

    (predict, confidence)
  }

  def runClassifier(classifier: Classifier, doubleFeatures: Seq[Double], stringFeatures: Seq[String], label: String) = {
    // load header
    val indexAttrList = BinaryClassification.readHeader()
    val attributes: FastVector[Attribute] = new FastVector()
    indexAttrList.foreach(p => { attributes.addElement(p._2) })
    val instances = new Instances("testing", attributes, 0)

    // create instances
    var instance = new DenseInstance(instances.numAttributes())
    var index = 0
    var error = ""
    doubleFeatures.foreach(p => { instance.setValue(index, p); index = index + 1 })
    try {
      stringFeatures.foreach(p => { instance.setValue(attributes.elementAt(index), p.replaceAll("\"", "")); index = index + 1; error = p; })
    } catch {
      case e: Exception => println("feature value is not defined: " + error)
    }
    instances.add(0, instance)
    instances.setClassIndex(instances.numAttributes() - 1)
    println(instance.toString())
    // run classifier
    try {
      //    val classindex = classifier.classifyInstance(instances.instance(0)).toInt
      val classindex: Int = 0
      val preValue = instances.instance(0).classAttribute().value(classindex) //preValue is the true name of prediction
      val preConfidence = classifier.distributionForInstance(instances.instance(0)).apply(classindex) //choose the confidence score of the prediction  
      (preValue, preConfidence)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println(s"Caught exception classifying test instance $instances")
        (-1d, -1d)
    }
  }
}