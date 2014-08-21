package org.allenai.relation.learning

import java.io.{ File, PrintWriter }
import java.util.Random
import scala.collection.mutable.Map
import scala.collection.immutable
import org.allenai.ari.solvers.inference.matching.{ EntailmentService, EntailmentWrapper }
import org.allenai.ari.solvers.utils.Tokenizer
import org.allenai.common.Logging
import weka.classifiers.{ Classifier, Evaluation }
import weka.classifiers.bayes._
import weka.classifiers.functions._
import weka.classifiers.meta._
import weka.classifiers.rules._
import weka.classifiers.trees._
import weka.core.Instances
import weka.core.converters.ConverterUtils.DataSource
import scala.io.Source
import scala.collection.immutable.IndexedSeq
import weka.core.Utils
import org.allenai.relation.util.Polyparser
import scala.collection.immutable.SortedMap

object BinaryClassification extends App with Logging {
  private val disrelSeeds = collection.immutable.HashMap(
    ("purpose", List("purpose", "used to", "responsible")),
    ("cause", List("caused", "so that", "because", "result in", "effect on")),
    ("effect", List("caused", "so that", "because", "result in", "effect on")),
    /**("function", List("used to")),**/
    ("example", List("an example of", "called", "a way to", "include", "such as")),
    ("enable", List("to help", "by")),
    ("part", List("part of")),
    ("requirement", List("necessary", "needed")),
    ("condition", List("when", "if")))

  // training and testing files
//    case "J48" => new J48()
//      case "RandomForest" => new RandomForest()
//      case "DecisionTable" => new DecisionTable()
//      case "REPTree" => new REPTree()
//      case "Logistic" => new Logistic()
//      case "SMO" => new SMO()
//      case "NaiveBayes" => new NaiveBayes()
//      case "JRip" => new JRip()
  val configClassifierName = "J48"
  val configFeature = "lexical-dependency-length"
//  val configFeature = "lexical"
  val configTrainingFile = "data/binary/train.txt"
//  val configTestingFile = "data/binary/inputDirectory/barrons.txt"
//  val configTestingLabeledFile = "data/binary/outputDirectory/barrons.txt"
  val configTestingFile = "data/binary/inputDirectory/108Q_sentence_arg1_arg2_mini_211.txt"
  val configTestingLabeledFile = "data/binary/outputDirectory/108Q_sentence_arg1_arg2_mini_211-" + configFeature + ".txt"
  val arffDir = "data/binary/arff"
  val mapDir = "data/binary/map"
  val configArffTrain = arffDir + File.separator + "train-" + configFeature + ".arff" 
  val configArffTest = arffDir + File.separator + "test-" + configFeature + ".arff"
  val configMapTrain = mapDir + File.separator + "train-" + configFeature + ".map"
  val configMapTest = mapDir + File.separator + "test-" + configFeature + ".map"

  // extract training sentences and features
  logger.info(s"Extracting training sentences+disrel from $configTrainingFile")
  val sentenceDisrelTrain = BinarySentenceDisrel.fromTrainingFile(configTrainingFile, 1)
  logger.info("Computing training sentence features")
  val featureDoubleMapTrain = sentenceDisrelTrain.map {
    sentenceDisrel => (sentenceDisrel, featuresDouble(sentenceDisrel))
  }.toMap
  val featureStringMapTrain = sentenceDisrelTrain.map {
    sentenceDisrel => (sentenceDisrel, featuresString(sentenceDisrel))
  }.toMap
  
  // extract testing sentences and features
  logger.info(s"Extracting testing sentences+disrel from $configTestingFile")
  val sentenceDisrelTest = BinarySentenceDisrel.fromTrainingFile(configTestingFile, 1)
  logger.info("Computing testing sentence features")
  val featureDoubleMapTest = sentenceDisrelTest.map {
    sentenceDisrel => (sentenceDisrel, featuresDouble(sentenceDisrel))
  }.toMap
  val featureStringMapTest = sentenceDisrelTest.map {
    sentenceDisrel => (sentenceDisrel, featuresString(sentenceDisrel))
  }.toMap

  // combine nominal features
  val nominalTitlesFromTrain = featureStringMapTrain.values
  val nominalTitlesFromTest = featureStringMapTest.values
  var nominalmapsUnsorted:Map[String, String] = collection.mutable.Map.empty[String, String]
  val featuresize = nominalTitlesFromTrain.toList(0).size
  for(i <- 0 to featuresize-1) {
    var nominals:Set[String] = Set()
    nominalTitlesFromTrain.foreach(p => nominals += p(i))
    nominalTitlesFromTest.foreach(p => nominals += p(i))
    nominalmapsUnsorted.put(i.toString, nominals.mkString(","))
  }
  val nominalmaps = SortedMap(nominalmapsUnsorted.toSeq:_*)
  
  // write training arff
  logger.info(s"Writing training ARFF to file $configArffTrain")
  toARFF(sentenceDisrelTrain, featureDoubleMapTrain, featureStringMapTrain, nominalmaps, configArffTrain)
  toMAP(sentenceDisrelTrain, featureDoubleMapTrain, featureStringMapTrain, configMapTrain)
  

//  runEvaluation(configClassifierName, configArffTrain, configArffTrain, true, Some(List()))
  runLabeling(configClassifierName, configArffTrain, featureDoubleMapTest, featureStringMapTest)
  System.exit(0)
  
  // 
  def runLabeling(configClassifierName:String, configArffTrain:String, 
      featureDoubleMapTest: collection.immutable.Map[BinarySentenceDisrel, Seq[Double]], 
      featureStringMapTest: collection.immutable.Map[BinarySentenceDisrel, Seq[String]]) = {
        val (classifier, eval) = buildClassifier(configClassifierName, configArffTrain)
        classify(classifier, eval, configArffTest, configTestingLabeledFile, sentenceDisrelTest, 
        featureDoubleMapTest, featureStringMapTest, nominalmaps)
  }

  def toInstances(arffFile: String, sentenceDisrelTest: List[BinarySentenceDisrel], 
      featureDoubleMapTest: collection.immutable.Map[BinarySentenceDisrel, Seq[Double]], 
      featureStringMapTest: collection.immutable.Map[BinarySentenceDisrel, Seq[String]], 
      nominalmaps: collection.immutable.Map[String, String]): (Instances, Seq[BinarySentenceDisrel]) = {
    toARFF(sentenceDisrelTest, featureDoubleMapTest, featureStringMapTest, nominalmaps, arffFile)
    toMAP(sentenceDisrelTest, featureDoubleMapTest, featureStringMapTest, configMapTest)
    
    logger.info(s"WEKA: reading test data from $arffFile")
    val sourceTest: DataSource = new DataSource(arffFile)
    val dataTest: Instances = sourceTest.getDataSet
    if (dataTest.classIndex == -1)
      dataTest.setClassIndex(dataTest.numAttributes - 1)
    (dataTest, sentenceDisrelTest)
  }

  def classify(classifier: Classifier, eval: Evaluation, testInstances: Instances) = {
    logger.info(s"WEKA: evaluating -- computing evaluation summary and matrix for ${testInstances.numInstances} test instances")
    eval.evaluateModel(classifier, testInstances)
    logger.info(s"Weka: Summary of Classification")
    println(eval.toSummaryString)
    logger.info(s"Weka: Confusion Matrix for Classification")
    println(eval.toMatrixString())
    logger.info(s"Weka: Precision / Recall")
    println("Precision for ClassIndex=0:\t" + eval.precision(0))
    println("Recall for ClassIndex=0:\t" + eval.recall(0))
    println("Precision for ClassIndex=1:\t" + eval.precision(1))
    println("Recall for ClassIndex=1:\t" + eval.recall(1))
    
    logger.info(s"WEKA: scoring -- computing class probability distribution for ${testInstances.numInstances} test instances")
    (0 to testInstances.numInstances - 1).map { i =>
      try {
        val preIndex = classifier.classifyInstance(testInstances.instance(i)).toInt //preIndex is the index of predication (as Double)
        val preValue = testInstances.instance(i).classAttribute().value(preIndex) //preValue is the true name of prediction
        val preConfidence = classifier.distributionForInstance(testInstances.instance(i)).apply(preIndex) //choose the confidence score of the prediction
        (preValue, preConfidence)
      } catch {
        case e: Exception =>
   //       e.printStackTrace()
          println(s"Caught exception classifying test instance $i")
          (-1d, -1d)
      }
    }
  }

  def classify(classifier: Classifier, eval: Evaluation, arffFile: String, outputFile: String, 
      sentenceDisrelTest: List[BinarySentenceDisrel], 
      featureDoubleMapTest: collection.immutable.Map[BinarySentenceDisrel, Seq[Double]], 
      featureStringMapTest: collection.immutable.Map[BinarySentenceDisrel, Seq[String]], 
      nominalmaps: collection.immutable.Map[String, String]): Unit = {

        try {
          logger.info(s"Processing ${configTestingFile}")
          val writer = new PrintWriter(outputFile, "utf-8")
          writer.println(BinarySentenceDisrel.header)
          var numPositiveExamples: Int = 0
          var numAllExamples: Int = 0
          toInstances(arffFile, sentenceDisrelTest, featureDoubleMapTest, featureStringMapTest, nominalmaps) match {
            case (testInstances, testSentenceDisrel) =>

              // compute prediction and confidence value
              val classProbabilities = classify(classifier, eval, testInstances)
              //  classProbabilities.foreach(p => println(p._1 + "\t" + p._2))
              val testSentenceDisrelWithClassProb = (testSentenceDisrel zip classProbabilities)
 
              testSentenceDisrelWithClassProb.foreach {
                x => writer.println(f"${x._1.toString}\t${x._2._1}\t${x._2._2}%.4f")
                if(x._1.annotationOpt.toList(0).toString().equals(x._2._1))
                  numPositiveExamples = numPositiveExamples+1
                numAllExamples = numAllExamples+1
              }
              println("correctly classified instance # = " + numPositiveExamples + " over " + numAllExamples)
              println("percentage of correct prediction = " + (numPositiveExamples*0.1)/(numAllExamples*0.1))
          }
          writer.close()
        } catch {
          case e: Exception =>
            e.printStackTrace()
            println(s"Caught exception processing input file ${configTestingFile}")
        }

  }
  
  def featuresString(sentenceDisrel: BinarySentenceDisrel) = {
    import FeatureWrapper._
    var features = Seq[String]()
    
    try {
      val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
      val ns = tree.vertices
      val ds = tree.edges    
      val arg1list = Polyparser.findHeadW(ns.toList, sentenceDisrel.arg1, ds.toList)
      val arg2list = Polyparser.findHeadW(ns.toList, sentenceDisrel.arg2, ds.toList)
      val rootstring = getrootstring(root)
      val shortestpathone = getshortestpath(root, tree, arg1list, arg2list, 1)
      val shortestpathtwo = getshortestpath(root, tree, arg1list, arg2list, 2)
      val shortestpaththree = getshortestpath(root, tree, arg1list, arg2list, 3)
      
//      features :+= rootstring
      if(configFeature.contains("-detail")) {
    	features :+= shortestpathone
        features :+= shortestpathtwo
        features :+= shortestpaththree        
      }
      if(configFeature.contains("-length")) {
        if(shortestpathone.equals("null")) features :+= "0"
        else features :+= "1"     
        if(shortestpathtwo.equals("null")) features :+= "0"
        else features :+= "1"
        if(shortestpaththree.equals("null")) features :+= "0"
        else features :+= "1"
      }
    } catch {
      case p => p.printStackTrace()
//      features :+= "null"
      if(configFeature.contains("-detail")) {
        features :+= "null"
        features :+= "null"
        features :+= "null"
      }
      
      if(configFeature.contains("-length")) {
        features :+= "0"
        features :+= "0"
        features :+= "0"
      }
    }
    
    features
  }
  
//  def featuresString(sentenceDisrel: BinarySentenceDisrel) = {
//    import FeatureWrapper._
//    var features = Seq[String]()
//    
////    try {
////      val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
////      val ns = tree.vertices
////      val ds = tree.edges    
////      val arg1list = Polyparser.findHeadW(ns.toList, sentenceDisrel.arg1, ds.toList)
////      val arg2list = Polyparser.findHeadW(ns.toList, sentenceDisrel.arg2, ds.toList)
////      
////      if(getshortestpath(root, tree, arg1list, arg2list, 1).equals("null")) features :+= "0"
////      else features :+= "1"
////      
////      if(getshortestpath(root, tree, arg1list, arg2list, 2).equals("null")) features :+= "0"
////      else features :+= "1"
////
////      if(getshortestpath(root, tree, arg1list, arg2list, 3).equals("null")) features :+= "0"
////      else features :+= "1"
////      
////    } catch {
////      case p => p.printStackTrace()
////      features :+= "0"
////      features :+= "0"
////      features :+= "0"
////        
////    }
//    
//    features
//  }

  def featuresDouble(sentenceDisrel: BinarySentenceDisrel) = {
    import FeatureWrapper._

    val sentenceSet = Tokenizer.toKeywords(sentenceDisrel.sentence).toSet
    val relphraseSet = Tokenizer.toKeywords(sentenceDisrel.relphrase).toSet
//    println(sentenceDisrel.disrel.toLowerCase())
//    println(sentenceDisrel.sentence)
    val disrelSet = Tokenizer.toKeywords(disrelSeeds(sentenceDisrel.disrel.toLowerCase()).mkString(" ")).toSet
    val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
    
    var features = Seq[Double]()
    // number of words in the sentence
    features :+= Math.log(sentenceDisrel.sentence.split("\\s+").size).toDouble

    // distance between arg1 and arg2
    features :+= Math.log(distance(sentenceDisrel.sentence, sentenceDisrel.arg1, sentenceDisrel.arg2)).toDouble

    // overlap between sentenceDisrel.relphrase and lexical-cue-seeds for "requirement"
    features :+= overlap(sentenceSet, disrelSet).toDouble

    // entailment between relphrase and disrel-lexical-seeds
    var entailmentscore = 0.0
    try { entailmentscore = wordnetEntailment(sentenceDisrel.sentence, disrelSet.mkString(" ")).toDouble
    } catch { case e:Exception => sentenceDisrel.sentence }
    features :+= entailmentscore
    
    // entailment between root and lexical seeds
    var entailmentroot = 0.0
    if(root != null) { 
      entailmentroot = wordnetEntailment(root.string, disrelSet.mkString(" ")).toDouble
    }
    features :+= entailmentroot
    
    features
  }
  
  def toMAP(sentenceDisrels: List[BinarySentenceDisrel], 
      featureDoubleMap: collection.immutable.Map[BinarySentenceDisrel, Seq[Double]], 
      featureStringMap: collection.immutable.Map[BinarySentenceDisrel, Seq[String]], 
      mapFile: String) = {
    val writer = new PrintWriter(mapFile)
    sentenceDisrels.foreach {
        sentenceDisrel =>
          writer.print(sentenceDisrel.toString + "\t")
          writer.print(featureDoubleMap(sentenceDisrel).mkString(",")+",")
          writer.println(featureStringMap(sentenceDisrel).mkString(",") + "," + sentenceDisrel.annotationOpt.toList(0)(0))
    }
    writer.close()
  }

  def toARFF(sentenceDisrels: List[BinarySentenceDisrel], 
      featureDoubleMap: collection.immutable.Map[BinarySentenceDisrel, Seq[Double]], 
      featureStringMap: collection.immutable.Map[BinarySentenceDisrel, Seq[String]], 
      nominalmap: collection.immutable.Map[String, String], 
      arffFile: String) = {
    
    val writer = new PrintWriter(arffFile)
    // add ARFF header
    writer.println("@relation SENTENCE_DISREL")
    writer.println("  @attribute sentence-length         numeric         % length of the sentence")
    writer.println("  @attribute distance-arg1-arg2      numeric         % distance between arg1 and arg2")
    writer.println("  @attribute overlap-sen-disrel      numeric         % word overlap between sentence and disrel-lexical-cue-seeds")
    writer.println("  @attribute entailment-sen-seeds    numeric         % word entailment for sentence and disrel-lexical-cue-seeds")
    writer.println("  @attribute entailment-root-seeds   numeric         % word entailment for root-string and disrel-lexiccal-cue-seeds")
    nominalmap.foreach(p => writer.println("  @attribute "+p._1 + "  {"+p._2+"}    % " + p._1))
    writer.println("  @attribute class                   {1,0}           % MULTI-CLASS LABEL: describe the discourse relation in the sentence")
    writer.println("")
    // add ARFF data
    writer.println("@data")
    sentenceDisrels.foreach {
      sentenceDisrel =>
        val annotation = sentenceDisrel.annotationOpt match {
          case Some(label: String) => if (Integer.parseInt(label) > 0) "1" else "0"
          case None => "?"
        }
        writer.print(featureDoubleMap(sentenceDisrel).mkString(",") + ",")
        writer.println(featureStringMap(sentenceDisrel).mkString(",") + "," + annotation)
    }
    writer.close()
  }

  def buildClassifier(classifierName: String, arffTrain: String) = {
    logger.info(s"WEKA: reading training data from $arffTrain")
    val sourceTrain: DataSource = new DataSource(arffTrain)
    val dataTrain: Instances = sourceTrain.getDataSet
    if (dataTrain.classIndex == -1)
      dataTrain.setClassIndex(dataTrain.numAttributes - 1)
    logger.info(s"WEKA: creating classifier $classifierName")
    val classifier: Classifier = classifierName match {
      case "J48" => new J48()
      case "RandomForest" => new RandomForest()
      case "DecisionTable" => new DecisionTable()
      case "REPTree" => new REPTree()
      case "Logistic" => new Logistic()
      case "SMO" => new SMO()
      case "NaiveBayes" => new NaiveBayes()
      case "JRip" => new JRip()
      //case "IBk" => new IBk()
//      case "RBFNetwork" => new RBFNetwork()
//      case "RotationForest" => new RotationForest()
//      case "ConjunctiveRule" => new ConjunctiveRule()
//      case "RandomCommittee" => new RandomCommittee()
//      case "LibSVM" => new LibSVM()
    }
    logger.info(s"WEKA: training the classifier on $arffTrain")
    classifier.buildClassifier(dataTrain)
    val eval = new Evaluation(dataTrain)
    (classifier, eval)
  }
  
  def runEvaluation(classifierName: String, arffTrain: String, arffValidation: String,
      isCrossValidation: Boolean, questionSentencesValidationOpt: Option[List[BinarySentenceDisrel]]) = {
    logger.info(s"WEKA: reading training data from $arffTrain")
    val sourceTrain: DataSource = new DataSource(arffTrain)
    val dataTrain: Instances = sourceTrain.getDataSet
    if (dataTrain.classIndex == -1)
      dataTrain.setClassIndex(dataTrain.numAttributes - 1)

    logger.info(s"WEKA: reading validation data from $arffValidation")
    val sourceValidation: DataSource = new DataSource(arffValidation)
    val dataValidation: Instances = sourceValidation.getDataSet
    if (dataValidation.classIndex == -1)
      dataValidation.setClassIndex(dataValidation.numAttributes - 1)

    logger.info(s"WEKA: creating classifier $classifierName")
    val classifier: Classifier = classifierName match {
      case "J48" => new J48()
      case "RandomForest" => new RandomForest()
      case "DecisionTable" => new DecisionTable()
      case "REPTree" => new REPTree()
      case "Logistic" => new Logistic()
      case "SMO" => new SMO()
      case "NaiveBayes" => new NaiveBayes()
      case "JRip" => new JRip()
      //case "IBk" => new IBk()
//      case "RBFNetwork" => new RBFNetwork()
//      case "RotationForest" => new RotationForest()
//      case "ConjunctiveRule" => new ConjunctiveRule()
//      case "RandomCommittee" => new RandomCommittee()
//      case "LibSVM" => new LibSVM()
    }

    val eval: Evaluation = new Evaluation(dataTrain)

    val classProbabilitiesOpt: Option[Seq[Array[Double]]] = isCrossValidation match {
      case true => 
        val nfolds: Int = 10
        logger.info(s"WEKA: training AND ${nfolds}-fold cross validating the classifier on $arffTrain")
        val random: Random = new Random(2)
        eval.crossValidateModel(classifier, dataTrain, 10, random)
        Option(null)
      case false =>
        logger.info(s"WEKA: training the classifier on $arffTrain")
        classifier.buildClassifier(dataTrain)
        logger.info(s"WEKA: validating the classifier on $arffValidation")
        eval.evaluateModel(classifier, dataValidation)

        logger.info("WARNING: duplicating the evaluation for now")
        logger.info(s"WEKA: extracting class probability distribution for each validation instance")
        val classProbabilities = (0 to dataValidation.numInstances - 1).map {
          i => classifier.distributionForInstance(dataValidation.instance(i))
        }
        Option(classProbabilities)
    }
    logger.info(eval.toSummaryString("\n======== RESULTS ========\n", false))
    //logger.info(s"\nf-measure = ${eval.fMeasure(0).toString}")

    classProbabilitiesOpt
  }
}
