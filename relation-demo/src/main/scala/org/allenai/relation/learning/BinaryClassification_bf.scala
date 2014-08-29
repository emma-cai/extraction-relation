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
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream
import scala.collection.mutable.Map
import weka.core.Attribute
import FeatureWrapper._
import weka.filters.unsupervised.attribute.RemoveUseless
import weka.filters.Filter
import weka.filters.unsupervised.attribute.NominalToBinary

object BinaryClassification_bf extends App with Logging {
  private var numericnum = 0
  var disrelSeeds = collection.mutable.Map(
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
  var configClassifierName = "Logistic"
  var configFeature = "lexical-prep-detail-length-new"
 // var configFeature = "lexical-version3"
  var configTrainingFile = "data/binary/train_mini.txt"
  //  val configTestingFile = "data/binary/inputDirectory/barrons.txt"
  //  val configTestingLabeledFile = "data/binary/outputDirectory/barrons.txt"
  val arffDir = "data/binary/"+configClassifierName+"/arff"
  val mapDir = "data/binary/"+configClassifierName+"/map"
  var modelDir = "data/binary/"+configClassifierName+"/model"
  val inputDir = "data/binary/inputDirectory"
  val outputDir = "data/binary/"+configClassifierName+"/outputDirectory"
//  val configTestingFile = inputDir+"/108Q_sentence_arg1_arg2_mini_211.txt"
  val configTestingFile = inputDir+"/barrons.txt"
  val configTestingLabeledFile = outputDir + "/108Q_sentence_arg1_arg2_mini_211-" + configFeature + ".txt"
  var configArffTrain = arffDir + File.separator + "train-" + configFeature + ".arff"
  val configArffTest = arffDir + File.separator + "test-" + configFeature + ".arff"
  val configMapTrain = mapDir + File.separator + "train-" + configFeature + ".map"
  val configMapTest = mapDir + File.separator + "test-" + configFeature + ".map"
  var configClassifierModel = modelDir + File.separator + configClassifierName + "-classifier-" + configFeature + ".classifier"
  var configEvalModel = modelDir + File.separator + configClassifierName + "-eval-" + configFeature + ".eval"

  
  // extract training sentences and features
  logger.info(s"Extracting training sentences+disrel from $configTrainingFile")
  val sentenceDisrelTrain = BinarySentenceDisrel.fromTrainingFile(configTrainingFile, 1)
  logger.info("Computing training sentence features")
  var featureDoubleMapTrain: Map[BinarySentenceDisrel, Seq[Double]] = collection.mutable.Map.empty
  var featureStringMapTrain: Map[BinarySentenceDisrel, Seq[String]] = collection.mutable.Map.empty

  // read testing data
  sentenceDisrelTrain.foreach {
    sentenceDisrel =>
      {
        val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
        val arg1list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg1, tree.edges.toList)
        val arg2list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg2, tree.edges.toList)
        val lengthDependenciesMap = getLengthDependencies(root, tree, arg1list, arg2list)
        featureDoubleMapTrain.put(sentenceDisrel, featuresDouble(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list))
        featureStringMapTrain.put(sentenceDisrel, featuresString(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list))
      }
  }

  // extract testing sentences and features
  logger.info(s"Extracting testing sentences+disrel from $configTestingFile")
  val sentenceDisrelTest = BinarySentenceDisrel.fromTrainingFile(configTestingFile, 1)
  logger.info("Computing testing sentence features")
  var featureDoubleMapTest: Map[BinarySentenceDisrel, Seq[Double]] = collection.mutable.Map.empty
  var featureStringMapTest: Map[BinarySentenceDisrel, Seq[String]] = collection.mutable.Map.empty
  sentenceDisrelTest.foreach {
    sentenceDisrel =>
      {
        val (root, tree) = Polyparser.processText(sentenceDisrel.sentence)
        val arg1list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg1, tree.edges.toList)
        val arg2list = Polyparser.findHeadW(tree.vertices.toList, sentenceDisrel.arg2, tree.edges.toList)
        val lengthDependenciesMap = getLengthDependencies(root, tree, arg1list, arg2list)
        featureDoubleMapTest.put(sentenceDisrel, featuresDouble(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list))
        featureStringMapTest.put(sentenceDisrel, featuresString(sentenceDisrel, root, tree, lengthDependenciesMap, arg1list, arg2list))
      }
  }

  // combine nominal features
  val nominalTitlesFromTrain = featureStringMapTrain.values
  val nominalTitlesFromTest = featureStringMapTest.values
  var nominalmapsUnsorted: Map[String, Set[String]] = collection.mutable.Map.empty[String, Set[String]]
  val featuresize = nominalTitlesFromTrain.toList(0).size
  for (i <- 0 to featuresize - 1) {
    var nominals: Set[String] = Set()
    nominalTitlesFromTrain.foreach(p => nominals += p(i))
    nominalTitlesFromTest.foreach(p => nominals += p(i))
    nominalmapsUnsorted.put(i.toString, nominals)
  }
  val nominalmaps = SortedMap(nominalmapsUnsorted.toSeq: _*)

  // write training arff
  logger.info(s"Writing training ARFF to file $configArffTrain")
  toARFF(sentenceDisrelTrain, featureDoubleMapTrain, featureStringMapTrain, nominalmaps, configArffTrain)
  toMAP(sentenceDisrelTrain, featureDoubleMapTrain, featureStringMapTrain, configMapTrain)

  // print (write) feature-coefficients
  printFeatureWeight(configClassifierName, configArffTrain)
  
  //Different ways for testing
  // save training-model
  saveModel(configClassifierName, configArffTrain, configClassifierModel, configEvalModel)
  runLabelingLoad(configClassifierModel, configEvalModel, featureDoubleMapTest, featureStringMapTest)

  //  // runEvaluation(configClassifierName, configArffTrain, configArffTrain, true, Some(List()))
  //  runLabelingRetrain(configClassifierName, configArffTrain, featureDoubleMapTest, featureStringMapTest)

  System.exit(0)

  
  // ********************************************************************************************************
  
  /**
   * initialize some common values based on different applications
   */
  def init(disrelseeds: collection.mutable.Map[String, List[String]], configclassifiername: String,
    configfeature: String, configclassifiermodel: String, configevalmodel: String, configarfftrain: String) = {
    disrelSeeds = disrelseeds
    configClassifierName = configclassifiername
    configFeature = configfeature
    configClassifierModel = configclassifiermodel
    configEvalModel = configevalmodel
    configArffTrain = configarfftrain
  }  
  
  /** Build a classifier (configClassifierName) using training-data (configArffTrain)
    * Save classifier-model to configClassifierModel
    * Save evaluation-model to configEvalModel
    */
  def saveModel(configclassifiername: String, configarfftrain: String, 
      configclassifiermodel:String, configevalmodel: String) = {
    val (classifier, eval) = buildClassifier(configclassifiername, configarfftrain)
    try {
      val oosclas = new ObjectOutputStream(new FileOutputStream(configclassifiermodel))
      oosclas.writeObject(classifier)
      oosclas.flush()
      oosclas.close()

      val ooseval = new ObjectOutputStream(new FileOutputStream(configevalmodel))
      ooseval.writeObject(eval)
      ooseval.flush()
      ooseval.close()
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }

  /** read header from arff file
    */
  def readHeader() = {
    val dataSource: DataSource = new DataSource(configArffTrain)
    val data: Instances = dataSource.getDataSet
    var indexAttrList: List[(Int, Attribute)] = List()
    for (a <- 0 to data.numAttributes() - 1) {
      indexAttrList = indexAttrList ::: List((a, data.attribute(a)))
    }
    indexAttrList
  }

  /** Load classifier-model from "configClassifierModel"
    * Load evaluation-model from "configEvalModel"
    */
  def loadModel(configClassifierModel: String, configEvalModel: String) = {
    try {
      val oisclas = new ObjectInputStream(new FileInputStream(configClassifierModel))
      val classifier = oisclas.readObject().asInstanceOf[Classifier]
      oisclas.close()

      val oiseval = new ObjectInputStream(new FileInputStream(configEvalModel))
      val eval = oiseval.readObject().asInstanceOf[Evaluation]
      oiseval.close()
      (classifier, eval)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        (null, null)
    }
  }

  /** Run testing, load pre-trained model in disk
    */
  def runLabelingLoad(configClassifierModel: String, configEvalModel: String,
    featureDoubleMapTest: Map[BinarySentenceDisrel, Seq[Double]],
    featureStringMapTest: Map[BinarySentenceDisrel, Seq[String]]) {
    val (classifier, eval) = loadModel(configClassifierModel, configEvalModel)
    classify(classifier, eval, configArffTest, configTestingLabeledFile, sentenceDisrelTest,
      featureDoubleMapTest, featureStringMapTest, nominalmaps)
  }

  /** Run testing, using the trained model at the same time
    */
  def runLabelingRetrain(configclassifiername: String, configarfftrain: String,
    featureDoubleMapTest: Map[BinarySentenceDisrel, Seq[Double]],
    featureStringMapTest: Map[BinarySentenceDisrel, Seq[String]]) = {
    val (classifier, eval) = buildClassifier(configclassifiername, configarfftrain)
    classify(classifier, eval, configArffTest, configTestingLabeledFile, sentenceDisrelTest,
      featureDoubleMapTest, featureStringMapTest, nominalmaps)
  }

  /** Convert data to instances
    */
  def toInstances(arffFile: String, sentenceDisrelTest: List[BinarySentenceDisrel],
    featureDoubleMapTest: Map[BinarySentenceDisrel, Seq[Double]],
    featureStringMapTest: Map[BinarySentenceDisrel, Seq[String]],
    nominalmaps: collection.immutable.Map[String, Set[String]]): (Instances, Seq[BinarySentenceDisrel]) = {
    toARFF(sentenceDisrelTest, featureDoubleMapTest, featureStringMapTest, nominalmaps, arffFile)
    toMAP(sentenceDisrelTest, featureDoubleMapTest, featureStringMapTest, configMapTest)

    logger.info(s"WEKA: reading test data from $arffFile")
    val sourceTest: DataSource = new DataSource(arffFile)
    val dataTest: Instances = sourceTest.getDataSet
    if (dataTest.classIndex == -1)
      dataTest.setClassIndex(dataTest.numAttributes - 1)
    (dataTest, sentenceDisrelTest)
  }

  /**
   * for testing
   */
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
    featureDoubleMapTest: Map[BinarySentenceDisrel, Seq[Double]],
    featureStringMapTest: Map[BinarySentenceDisrel, Seq[String]],
    nominalmaps: collection.immutable.Map[String, Set[String]]): Unit = {

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
            x =>
              writer.println(f"${x._1.toString}\t${x._2._1}\t${x._2._2}%.4f")
              if (x._1.annotationOpt.toList(0).toString().equals(x._2._1))
                numPositiveExamples = numPositiveExamples + 1
              numAllExamples = numAllExamples + 1
          }
          println("correctly classified instance # = " + numPositiveExamples + " over " + numAllExamples)
          println("percentage of correct prediction = " + (numPositiveExamples * 0.1) / (numAllExamples * 0.1))
      }
      writer.close()
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println(s"Caught exception processing input file ${configTestingFile}")
    }

  }
  
  /**
   * get nominal features
   */
  def featuresString(sentenceDisrel: BinarySentenceDisrel,
    root: Polyparser.Mytokennode, tree: Polyparser.Mygraph,
    lengthDependenciesMap: Map[Integer, List[(Int, Int, Set[Polyparser.Myedge])]],
    arg1list: List[Int], arg2list: List[Int]) = {
    import FeatureWrapper._
    var features = Seq[String]()

    try {
      val length1 = lengthDependenciesMap.apply(1).size
      val length2 = lengthDependenciesMap.apply(2).size
      val length3 = lengthDependenciesMap.apply(3).size

      // specific dependency path
      if (configFeature.contains("-detail")) {
        val generalDependenciesLength1 = getGeneralDependencySets(lengthDependenciesMap.apply(1)) // general-dependency-path = 1
        val generalDependenciesLength2 = getGeneralDependencySets(lengthDependenciesMap.apply(2)) // general-dependency-path = 2
        val generalDependenciesLength3 = getGeneralDependencySets(lengthDependenciesMap.apply(3)) // general-dependency-path = 3
        if (generalDependenciesLength1.size == 0) features :+= "null"
        else features :+= "\"" + sentenceDisrel.disrel + "=>(" + generalDependenciesLength1(0).mkString(", ")+")" + "\""
        if (generalDependenciesLength2.size == 0) features :+= "null"
        else features :+= "\"" + sentenceDisrel.disrel + "=>(" + generalDependenciesLength2(0).mkString(", ")+")" + "\""
        if (generalDependenciesLength3.size == 0) features :+= "null"
        else features :+= "\"" + sentenceDisrel.disrel + "=>(" + generalDependenciesLength3(0).mkString(", ")+")" + "\""
      }

      if (configFeature.contains("-length")) {
        // shortest-dependency-path-length
        val shortestpath = Math.min(length1, Math.min(length2, length3))
        features :+= shortestpath.toString

        // Does exist a path = 1?
        if (length1 != 0) features :+= "\""+sentenceDisrel.disrel + "=>1" + "\"" 
        else features :+= "\"" + sentenceDisrel.disrel + "=>0" + "\""

        // Does exist a path = 2?
        if (length2 != 0) features :+= "\""+sentenceDisrel.disrel + "=>1" + "\"" 
        else features :+= "\"" + sentenceDisrel.disrel + "=>0" + "\""

        // Dees exist a path = 3?
        if (length3 != 0) features :+= "\""+sentenceDisrel.disrel + "=>1" + "\"" 
        else features :+= "\"" + sentenceDisrel.disrel + "=>0" + "\""
      }

      // If there exists prep(x, y), what is pos(y)
      if (configFeature.contains("-prep")) {
        val lemmaofprep = getlemmaofprep(lengthDependenciesMap)
        features :+= lemmaofprep.apply("for").toString
        features :+= lemmaofprep.apply("of").toString
        features :+= lemmaofprep.apply("to").toString
      }
    } catch {
      case p: Throwable => {
        p.printStackTrace()
        if (configFeature.contains("-detail")) {
          features :+= "notree" // specific-dependency-path with length=1
          features :+= "notree" // specific-dependency-path with length=2
          features :+= "notree" // specific-dependency-path with length=3
        }

        if (configFeature.contains("-length")) {
          features :+= "notree" // shortest-dependency-path-length
          features :+= "notree" // 0: no-dependency-path with length=1
          features :+= "notree" // 0: no-dependency-path with length=2
          features :+= "notree" // 0: no-dependency-path with length=3            
        }

        if (configFeature.contains("-prep")) {
          features :+= "notree"
          features :+= "notree"
          features :+= "notree"
        }
      }
    }

    features
  }

  /**
   * get numeric features
   */
  def featuresDouble(sentenceDisrel: BinarySentenceDisrel,
    root: Polyparser.Mytokennode, tree: Polyparser.Mygraph,
    lengthDependenciesMap: Map[Integer, List[(Int, Int, Set[Polyparser.Myedge])]],
    arg1list: List[Int], arg2list: List[Int]) = {
    import FeatureWrapper._

    numericnum = 0
    val sentenceSet = Tokenizer.toKeywords(sentenceDisrel.sentence).toSet
    //   val relphraseSet = Tokenizer.toKeywords(sentenceDisrel.relphrase).toSet
    val seedSet = Tokenizer.toKeywords(disrelSeeds(sentenceDisrel.disrel.toLowerCase()).mkString(" ")).toSet

    var features = Seq[Double]()
    // number of words in the sentence
    features :+= Math.log(sentenceDisrel.sentence.split("\\s+").size).toDouble
    numericnum = numericnum + 1

    // distance between arg1 and arg2
    features :+= Math.log(distance(sentenceDisrel.sentence, sentenceDisrel.arg1, sentenceDisrel.arg2)).toDouble
    numericnum = numericnum + 1

    // overlap between sentenceDisrel.relphrase and lexical-cue-seeds for "requirement"
    features :+= overlap(sentenceSet, seedSet).toDouble
    numericnum = numericnum + 1

    // entailment between relphrase and disrel-lexical-seeds
    var entailmentscore = 0.0
    try {
      entailmentscore = wordnetEntailment(sentenceDisrel.sentence, seedSet.mkString(" ")).toDouble
    } catch { case e: Exception => sentenceDisrel.sentence }
    features :+= entailmentscore
    numericnum = numericnum + 1

    // entailment between root and lexical seeds
    var entailmentroot = 0.0
    if (root != null) {
      entailmentroot = wordnetEntailment(root.string, seedSet.mkString(" ")).toDouble
    }
    features :+= entailmentroot
    numericnum = numericnum + 1

    // entailment-score (disrel-seeds, connection-words)
    var entailmentconnection = 0.0
    val connectwords: Set[String] = getconnectwords(lengthDependenciesMap)
    if (connectwords != null) {
      entailmentconnection = wordnetEntailment(connectwords.mkString(" "), seedSet.mkString(" ")).toDouble
    }
    features :+= entailmentconnection
    numericnum = numericnum + 1

    features
  }

  /**
   * write data to map(instance, features)
   */
  def toMAP(sentenceDisrels: List[BinarySentenceDisrel],
    featureDoubleMap: Map[BinarySentenceDisrel, Seq[Double]],
    featureStringMap: Map[BinarySentenceDisrel, Seq[String]],
    mapFile: String) = {
    val writer = new PrintWriter(mapFile)
    sentenceDisrels.foreach {
      sentenceDisrel =>
        writer.print(sentenceDisrel.toString + "\t")
        writer.print(featureDoubleMap(sentenceDisrel).mkString(",") + ",")
        writer.println(featureStringMap(sentenceDisrel).mkString(",") + "," + sentenceDisrel.annotationOpt.toList(0)(0))
    }
    writer.close()
  }

  /**
   * write data to ARFF format
   */
  def toARFF(sentenceDisrels: List[BinarySentenceDisrel],
    featureDoubleMap: Map[BinarySentenceDisrel, Seq[Double]],
    featureStringMap: Map[BinarySentenceDisrel, Seq[String]],
    nominalmap: collection.immutable.Map[String, Set[String]],
    arffFile: String) = {

    val writer = new PrintWriter(arffFile)
    // add ARFF header
    writer.println("@relation SENTENCE_DISREL")
    for (i <- 1 to numericnum) { writer.println("  @attribute numeric-" + i + "      numeric      % " + i) }
    nominalmap.foreach(p => writer.println("  @attribute nominal-" + p._1 + "  {" + p._2.mkString(",") + "}    % " + p._1))
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

  /**
   * build classifier
   */
  def buildClassifier(configclassifiername: String, configarfftrain: String) = {
    logger.info(s"WEKA: reading training data from $configclassifiername")
    val sourceTrain: DataSource = new DataSource(configarfftrain)
    val dataTrain: Instances = sourceTrain.getDataSet
    if (dataTrain.classIndex == -1)
      dataTrain.setClassIndex(dataTrain.numAttributes - 1)
    logger.info(s"WEKA: creating classifier $configclassifiername")
    val classifier: Classifier = configclassifiername match {
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
    
    logger.info(s"WEKA: training the classifier on $configarfftrain")
    classifier.buildClassifier(dataTrain)
    val eval = new Evaluation(dataTrain)
    (classifier, eval)
  }
  
  /**
   * print feature-weights
   */
  def printFeatureWeight(configclassifiername: String, configarfftrain: String) = {
//    val (classifier, eval) = buildClassifier(configclassifiername, configarfftrain)
//    val tmp = classifier.asInstanceOf[Logistic]
//    val coeffs = tmp.coefficients()
    
//    for(i <- 0 to coeffs.length-1) {
//      for(j <- 0 to coeffs(0).length-1) {
//        print("i = " + i + ", j = " + j+ ", coeff = " + coeffs(i)(j) + "\t")
//      }
//      println()
//    }
    
    logger.info(s"WEKA: compute feature coefficients $configclassifiername");
    logger.info(s"WEKA: reading training data from $configclassifiername")
    val sourceTrain: DataSource = new DataSource(configarfftrain)
    val dataTrain: Instances = sourceTrain.getDataSet
    if (dataTrain.classIndex == -1)
      dataTrain.setClassIndex(dataTrain.numAttributes - 1)
    logger.info(s"WEKA: creating classifier $configclassifiername")
    val classifier: Classifier = configclassifiername match {
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
    // only for logistic regression model
   
    val logit_filter = new RemoveUseless()
    logit_filter.setInputFormat(dataTrain)
    var logit_filtered = Filter.useFilter(dataTrain, logit_filter)
    val nominaltobinary = new NominalToBinary()
    nominaltobinary.setInputFormat(logit_filtered)
    logit_filtered = Filter.useFilter(logit_filtered, nominaltobinary)
    
    logger.info(s"WEKA: training the classifier on filtered datata $configarfftrain")
    classifier.buildClassifier(logit_filtered)
    val eval = new Evaluation(logit_filtered)
    
    
    
    val tmp = classifier.asInstanceOf[Logistic]
    val coeffs = tmp.coefficients()
    println("size = " + coeffs.size)
    println("length = " + coeffs.length)
    for(i <- 0 to coeffs.length-1) {
      println("i="+i + "\t" + logit_filtered.attribute(i).name + "\t" + coeffs.apply(i)(0))
    }
  }

  /**
   * run 10-folds cross validation
   */
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
