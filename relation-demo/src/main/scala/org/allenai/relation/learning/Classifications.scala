//package org.allenai.relation.learning
//
//
//import java.io.{ File, PrintWriter }
//import java.util.Random
//import org.allenai.ari.solvers.inference.matching.{ EntailmentService, EntailmentWrapper }
//import org.allenai.ari.solvers.utils.Tokenizer
//import org.allenai.common.Logging
//import weka.classifiers.{ Classifier, Evaluation }
//import weka.classifiers.bayes._
//import weka.classifiers.functions._
//import weka.classifiers.meta._
//import weka.classifiers.rules._
//import weka.classifiers.trees._
//import weka.core.Instances
//import weka.core.converters.ConverterUtils.DataSource
//import scala.io.Source
//import scala.collection.immutable.IndexedSeq
//import weka.core.Utils
//
//
//class Classifications extends App with Logging {
//  val configClassifierName = "J48"
//  val configTrainingFile = "data/binary/all.txt"
//  val inputDirectory = "data/binary/inputDirectory"
//  val outputDirectory = "data/binary/outputDirectory"
//  
//  def runLabeling(configClassifierName:String, configArffTrain:String) = {
//    val classifier: Classifier = buildClassifier(configClassifierName, configArffTrain)
//    classify(classifier, inputDirectory, outputDirectory)
//  }
//
//  def toInstances(file: File, arffDir: String): (Instances, Seq[BinarySentenceDisrel]) = {
//    logger.info(s"Extracting test sentence+disrel from $file")
//    val sentenceDisrel: List[BinarySentenceDisrel] = BinarySentenceDisrel.fromTrainingFile(file.getAbsolutePath, 1)
//    
//    val featureMap: Map[BinarySentenceDisrel, Seq[Double]] = (sentenceDisrel map {
//      sentenceDisrel =>
//        (sentenceDisrel, features(sentenceDisrel))
//    }).toMap
//    val arffFile = arffDir + File.separator + file.getName + ".arff"
//    toARFF(sentenceDisrel, featureMap, arffFile)
//    logger.info(s"WEKA: reading test data from $arffFile")
//    val sourceTest: DataSource = new DataSource(arffFile)
//    val dataTest: Instances = sourceTest.getDataSet
//    if (dataTest.classIndex == -1)
//      dataTest.setClassIndex(dataTest.numAttributes - 1)
//    (dataTest, sentenceDisrel)
//  }
//
//  def classify(classifier: Classifier, testInstances: Instances) = {
//    logger.info(s"WEKA: scoring -- computing class probability distribution for ${testInstances.numInstances} test instances")
//    (0 to testInstances.numInstances - 1).map { i =>
//      try {
//        val preIndex = classifier.classifyInstance(testInstances.instance(i)).toInt //preIndex is the index of predication (as Double)
//        val preValue = testInstances.instance(i).classAttribute().value(preIndex) //preValue is the true name of prediction
//        val preConfidence = classifier.distributionForInstance(testInstances.instance(i)).apply(preIndex) //choose the confidence score of the prediction
//        (preValue, preConfidence)
//      } catch {
//        case e: Exception =>
//          e.printStackTrace()
//          println(s"Caught exception classifying test instance $i")
//          (-1d, -1d)
//      }
//    }
//  }
//
//  def classify(classifier: Classifier, inputDirectory: String, outputDirectory: String): Unit = {
//    val files = {
//      new File(inputDirectory).listFiles().filter(_.getName.endsWith(".arff"))
//    }
//
//    files.take(1).foreach {
//      file =>
//        try {
//          logger.info(s"Processing ${file.getName}")
//          val outputFile = outputDirectory + File.separator + file.getName
//          val writer = new PrintWriter(outputFile, "utf-8")
//          writer.println(BinarySentenceDisrel.header)
//          toInstances(file, arffDir) match {
//            case (testInstances, testSentenceDisrel) =>
//
//              // compute prediction and confidence value
//              val classProbabilities = classify(classifier, testInstances)
//              //  classProbabilities.foreach(p => println(p._1 + "\t" + p._2))
//              val testSentenceDisrelWithClassProb = (testSentenceDisrel zip classProbabilities)
//              testSentenceDisrelWithClassProb.foreach {
//                x => writer.println(f"${x._1.toString}\t${x._2._1}\t${x._2._2}%.4f")
//              }
//            case _ =>
//              logger.info("")
//          }
//          writer.close()
//        } catch {
//          case e: Exception =>
//            e.printStackTrace()
//            println(s"Caught exception processing input file ${file.getName}")
//        }
//    }
//  }
//  
//  def buildClassifier(classifierName: String, arffTrain: String) = {
//    logger.info(s"WEKA: reading training data from $arffTrain")
//    val sourceTrain: DataSource = new DataSource(arffTrain)
//    val dataTrain: Instances = sourceTrain.getDataSet
//    if (dataTrain.classIndex == -1)
//      dataTrain.setClassIndex(dataTrain.numAttributes - 1)
//    logger.info(s"WEKA: creating classifier $classifierName")
//    val classifier: Classifier = classifierName match {
//      case "J48" => new J48()
//      case "RandomForest" => new RandomForest()
//      case "DecisionTable" => new DecisionTable()
//      case "REPTree" => new REPTree()
//      case "Logistic" => new Logistic()
//      case "SMO" => new SMO()
//      case "NaiveBayes" => new NaiveBayes()
//      case "JRip" => new JRip()
//      //case "IBk" => new IBk()
////      case "RBFNetwork" => new RBFNetwork()
////      case "RotationForest" => new RotationForest()
////      case "ConjunctiveRule" => new ConjunctiveRule()
////      case "RandomCommittee" => new RandomCommittee()
////      case "LibSVM" => new LibSVM()
//    }
//    logger.info(s"WEKA: training the classifier on $arffTrain")
//    classifier.buildClassifier(dataTrain)
//    classifier
//  }
//}