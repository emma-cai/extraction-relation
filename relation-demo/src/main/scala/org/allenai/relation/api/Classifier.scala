package org.allenai.relation.api

import org.allenai.relation.learning.BinaryClassification
import java.io.File
import weka.core.Instance
import weka.core.converters.ArffLoader
import weka.core.Instances
import weka.core.FastVector
import java.util.ArrayList
import weka.core.Attribute
import org.allenai.relation.util.Polyparser
import org.allenai.relation.learning.BinaryClassification
import org.allenai.relation.learning.BinarySentenceDisrel
import org.allenai.relation.learning.FeatureWrapper._
import weka.classifiers.Classifier
import weka.core.DenseInstance

object Classifier {
  private val disrelSeeds = collection.mutable.Map(
    ("purpose", List("purpose", "used to", "responsible")),
    ("cause", List("caused", "so that", "because", "result in", "effect on")),
    ("effect", List("caused", "so that", "because", "result in", "effect on")),
    /**("function", List("used to")),**/
    ("example", List("an example of", "called", "a way to", "include", "such as")),
    ("enable", List("to help", "by")),
    ("part", List("part of")),
    ("requirement", List("necessary", "needed")),
    ("condition", List("when", "if")))
  val configClassifierName = "J48"
  val configFeature = "lexical-detail-length-version3"
  val modelDir = "data/binary/model"
  val arffDir = "data/binary/arff"
  val configArffTrain = arffDir + File.separator + "train-" + configFeature + ".arff"
  val configClassifierModel = modelDir + File.separator + configClassifierName + "-classifier-" + configFeature + ".classifier"
  val configEvalModel = modelDir + File.separator + configClassifierName + "-eval-" + configFeature + ".eval"



  def run(sentence: String, arg1: String, arg2: String) = {
    BinaryClassification.init(disrelSeeds, configClassifierName,
      configFeature, configClassifierModel, configEvalModel, configArffTrain)
    var predictlist: List[String] = List()
    var confidencelist: List[Double] = List()
    disrelSeeds.foreach {
      case p =>
        val (predict, confidence) = testInstance(sentence, p._1, arg1, arg2, "1")
        println(sentence + "\t" + arg1 + "\t" + arg2)
        println(p._1 + "\t" + confidence)
        println()
        predictlist = predictlist ::: List(p._1)
        confidencelist = confidencelist ::: List(confidence)
    }
    val pair = predictlist.zip(confidencelist)
    val pairsorted = pair.sortWith(_._2 > _._2)
    var predictlistsorted:List[String] = List()
    var confidencelistsorted:List[String] = List()
    pairsorted.foreach(p => {predictlistsorted = predictlistsorted:::List(p._1); 
    		confidencelistsorted = confidencelistsorted:::List(p._2.toString)})
    
    
    (predictlistsorted, confidencelistsorted)
  }

  def testInstance(sentence: String, disrel: String, arg1: String, arg2: String, label: String) = {
    // load trained model
    val (classifier, eval) = BinaryClassification.loadModel(configClassifierModel, configEvalModel)

    // extract testing sentences and features
    val sentenceDisrel = BinarySentenceDisrel.fromSingleInstance(sentence, disrel, arg1, arg2, label)
    val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
    val arg1list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg1, tree.edges.toList)
    val arg2list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg2, tree.edges.toList)
    val lengthDependenciesMap = getLengthDependencies(root, tree, arg1list, arg2list)
    val doubleFeatures: Seq[Double] = BinaryClassification.featuresDouble(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list)
    val stringFeatures: Seq[String] = BinaryClassification.featuresString(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list)

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
      stringFeatures.foreach(p => { instance.setValue(attributes.elementAt(index), p.replaceAll("\"", "")); index = index + 1; error = p })
    } catch {
      case e: Exception => println("feature value is not defined: " + error)
    }
    instances.add(0, instance)
    instances.setClassIndex(instances.numAttributes() - 1)
    println(instance.toString())
    // run classifier
    try {
      val classindex = 0

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