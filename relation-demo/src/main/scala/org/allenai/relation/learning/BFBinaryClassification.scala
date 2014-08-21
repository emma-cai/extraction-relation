package org.allenai.relation.learning

import java.io.{ File, PrintWriter }
import java.util.Random
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

object BFBinaryClassification extends App with Logging {
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

  val configClassifierName = "J48"
//  val configTrainingFile = "data/binary/all.txt"
  val configTrainingFile = "data/binary/inputDirectory/barrons.txt"
  val inputDirectory = "data/binary/inputDirectory"
  val outputDirectory = "data/binary/outputDirectory"
  val arffDir = "data/binary/arff"
  val configArffTrain = arffDir + File.separator + "train.arff"
  

  logger.info(s"Extracting training sentences+disrel from $configTrainingFile")
  val sentenceDisrelTrain = BinarySentenceDisrel.fromTrainingFile(configTrainingFile, 1)
  logger.info("Computing training sentence features")
  val featureMapTrain = sentenceDisrelTrain.map {
    sentenceDisrel => (sentenceDisrel, features(sentenceDisrel))
  }.toMap

  logger.info(s"Writing training ARFF to file $configArffTrain")
  toARFF(sentenceDisrelTrain, featureMapTrain, configArffTrain)

//  runEvaluation(configClassifierName, configArffTrain, configArffTrain, true, Some(List()))
  runLabeling(configClassifierName, configArffTrain)
  System.exit(0)
  
  def runLabeling(configClassifierName:String, configArffTrain:String) = {
    val classifier: Classifier = buildClassifier(configClassifierName, configArffTrain)
    classify(classifier, inputDirectory, outputDirectory)
  }

  def toInstances(file: File, arffDir: String): (Instances, Seq[BinarySentenceDisrel]) = {
    logger.info(s"Extracting test sentence+disrel from $file")
    //val questionSentences: List[QuestionSentence] = QuestionSentence.fromFileWithSids(file.getAbsolutePath, 0)
    val sentenceDisrel: List[BinarySentenceDisrel] = BinarySentenceDisrel.fromTrainingFile(file.getAbsolutePath, 1)
    //val questionSentences: List[QuestionSentence] = QuestionSentence.fromFileWithSidsLASTMINUTE(file.getAbsolutePath, 0)
    val featureMap: Map[BinarySentenceDisrel, Seq[String]] = (sentenceDisrel map {
      sentenceDisrel =>
        (sentenceDisrel, features(sentenceDisrel))
    }).toMap
    val arffFile = arffDir + File.separator + file.getName + ".arff"
    toARFF(sentenceDisrel, featureMap, arffFile)
    logger.info(s"WEKA: reading test data from $arffFile")
    val sourceTest: DataSource = new DataSource(arffFile)
    val dataTest: Instances = sourceTest.getDataSet
    if (dataTest.classIndex == -1)
      dataTest.setClassIndex(dataTest.numAttributes - 1)
    (dataTest, sentenceDisrel)
  }

  def classify(classifier: Classifier, testInstances: Instances) = {
    logger.info(s"WEKA: scoring -- computing class probability distribution for ${testInstances.numInstances} test instances")
    (0 to testInstances.numInstances - 1).map { i =>
      try {
        val preIndex = classifier.classifyInstance(testInstances.instance(i)).toInt //preIndex is the index of predication (as Double)
        val preValue = testInstances.instance(i).classAttribute().value(preIndex) //preValue is the true name of prediction
        val preConfidence = classifier.distributionForInstance(testInstances.instance(i)).apply(preIndex) //choose the confidence score of the prediction
        (preValue, preConfidence)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          println(s"Caught exception classifying test instance $i")
          (-1d, -1d)
      }
    }
  }

  def classify(classifier: Classifier, inputDirectory: String, outputDirectory: String): Unit = {
    val files = {
      new File(inputDirectory).listFiles().filter(_.getName.endsWith(".txt"))
    }

    files.take(1).foreach {
      file =>
        try {
          logger.info(s"Processing ${file.getName}")
          val outputFile = outputDirectory + File.separator + file.getName
          val writer = new PrintWriter(outputFile, "utf-8")
          writer.println(BinarySentenceDisrel.header)
          toInstances(file, arffDir) match {
            case (testInstances, testSentenceDisrel) =>

              // compute prediction and confidence value
              val classProbabilities = classify(classifier, testInstances)
              //  classProbabilities.foreach(p => println(p._1 + "\t" + p._2))
              val testSentenceDisrelWithClassProb = (testSentenceDisrel zip classProbabilities)
              testSentenceDisrelWithClassProb.foreach {
                x => writer.println(f"${x._1.toString}\t${x._2._1}\t${x._2._2}%.4f")
              }

              // compute average precision if the data is annotated
//              var numPositiveExamples: Int = 0
//              var averagePrecision: Double = 0d
//              (0 to testSentenceDisrelWithClassProb.size - 1) map {
//                i =>
//                  if (testSentenceDisrelWithClassProb(i)._2._1.equals(testSentenceDisrelWithClassProb(i)._1.annotationOpt.get)) {
//                    numPositiveExamples += 1
//                    averagePrecision += (numPositiveExamples.toDouble / (i + 1) * 100d)
//                  }
//              }
//              averagePrecision /= numPositiveExamples
//              logger.info(s"${numPositiveExamples} positive examples out of ${testSentenceDisrelWithClassProb.size} test instances")
//              logger.info(f"AVERAGE PRECISION = ${averagePrecision}%.4f")
            case _ =>
              logger.info("")
          }
          writer.close()
        } catch {
          case e: Exception =>
            e.printStackTrace()
            println(s"Caught exception processing input file ${file.getName}")
        }
    }
  }

  def features(sentenceDisrel: BinarySentenceDisrel) = {
    import FeatureWrapper._

    val sentenceSet = Tokenizer.toKeywords(sentenceDisrel.sentence).toSet
    val relphraseSet = Tokenizer.toKeywords(sentenceDisrel.relphrase).toSet
    val disrelSet = Tokenizer.toKeywords(disrelSeeds(sentenceDisrel.disrel.toLowerCase()).mkString(" ")).toSet
    val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
    
    var features = Seq[String]()
    // number of words in the sentence
    features :+= Math.log(sentenceDisrel.sentence.split("\\s+").size).toString

    // distance between arg1 and arg2
    features :+= Math.log(distance(sentenceDisrel.sentence, sentenceDisrel.arg1, sentenceDisrel.arg2)).toString

    // overlap between sentenceDisrel.relphrase and lexical-cue-seeds for "requirement"
    features :+= overlap(sentenceSet, disrelSet).toString

    // entailment between relphrase and disrel-lexical-seeds
    var entailmentscore = "0.0"
    try { entailmentscore = wordnetEntailment(sentenceDisrel.sentence, disrelSet.mkString(" ")).toString
    } catch { case e:Exception => sentenceDisrel.sentence }
    features :+= entailmentscore
    
    // entailment between root and lexical seeds
    features :+= wordnetEntailment(root.string, disrelSet.mkString(" ")).toString
    
    // dependency-path features
    features :+= sentenceDisrel.relphrase
    
    features
  }

  def toARFF(sentenceDisrels: List[BinarySentenceDisrel], featureMap: Map[BinarySentenceDisrel, Seq[String]], arffFile: String) = {
    var res:Set[String] = Set()
    var shortestpathrootarg1:Set[String] = Set()
    featureMap.values.foreach(p => shortestpathrootarg1 += p(5))
    val shortestpathrootarg1title = "{"+shortestpathrootarg1.mkString(",")+"}"
    
    val writer = new PrintWriter(arffFile)
    // add ARFF header
    writer.println("@relation SENTENCE_DISREL")
    writer.println("  @attribute sentence-length         numeric         % length of the sentence")
    writer.println("  @attribute distance-arg1-arg2      numeric         % distance between arg1 and arg2")
    writer.println("  @attribute overlap-sen-disrel      numeric         % word overlap between sentence and disrel-lexical-cue-seeds")
    writer.println("  @attribute entailment-sen-seeds    numeric         % word entailment for sentence and disrel-lexical-cue-seeds")
    writer.println("  @attribute entailment-root-seeds   numeric         % word entailment for root-string and disrel-lexiccal-cue-seeds")
    writer.println("  @attribute shortest-path-root-arg1 " + shortestpathrootarg1title + "   % shortest path between root and arg1")
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
        writer.println(featureMap(sentenceDisrel).mkString(",") + "," + annotation)
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
    classifier
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
