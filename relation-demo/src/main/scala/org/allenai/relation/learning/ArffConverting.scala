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

class ArffConverting extends App with Logging {
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

  private var textfile = ""
  private var arfffile = ""

  def ArffConverting(textfile: String, arfffile: String) = {
    this.textfile = textfile
    this.arfffile = arfffile
  }

  logger.info(s"Extracting training sentences+disrel from $textfile")
  val sentenceDisrelTrain = BinarySentenceDisrel.fromTrainingFile(textfile, 1)

  logger.info("Computing training sentence features")
  val featureMapTrain = sentenceDisrelTrain.map {
    sentenceDisrel => (sentenceDisrel, features(sentenceDisrel))
  }.toMap

  logger.info(s"Writing training ARFF to file $arfffile")
  toARFF(sentenceDisrelTrain, featureMapTrain, arfffile)

  /** Feature collecting
    */
  def features(sentenceDisrel: BinarySentenceDisrel) = {
    import FeatureWrapper._

    val sentenceSet = Tokenizer.toKeywords(sentenceDisrel.sentence).toSet
    val relphraseSet = Tokenizer.toKeywords(sentenceDisrel.relphrase).toSet
    val disrelSet = Tokenizer.toKeywords(disrelSeeds(sentenceDisrel.disrel.toLowerCase()).mkString(" ")).toSet

    var features = Seq[Double]()
    // number of words in the sentence
    features :+= Math.log(sentenceDisrel.sentence.split("\\s+").size.toDouble)

    // distance between arg1 and arg2
    features :+= Math.log(distance(sentenceDisrel.sentence, sentenceDisrel.arg1, sentenceDisrel.arg2))

    // overlap between sentenceDisrel.relphrase and lexical-cue-seeds for "requirement"
    features :+= overlap(sentenceSet, disrelSet).toDouble

    // entailment between relphrase and "requirement" lexical seeds
    var entailmentscore = 0.0
    try {
      entailmentscore = wordnetEntailment(sentenceDisrel.sentence, disrelSet.mkString(" "))
    } catch { case e: Exception => sentenceDisrel.sentence }
    features :+= entailmentscore

    // dependency-path features

    features
  }

  /** Write to arff file
    */
  def toARFF(sentenceDisrels: List[BinarySentenceDisrel], featureMap: Map[BinarySentenceDisrel, Seq[Double]], arffFile: String) = {
    val writer = new PrintWriter(arffFile)
    // add ARFF header
    writer.println("@relation SENTENCE_DISREL")
    writer.println("  @attribute sentence-length         numeric         % length of the sentence")
    writer.println("  @attribute distance-arg1-arg2      numeric         % distance between arg1 and arg2")
    writer.println("  @attribute overlap-disrel          numeric         % word overlap between relphrase and disrel lexical cue seeds")
    writer.println("  @attribute entailment-disrel      numeric         % word entailment for relphrase in disrel lexical cue seeds")
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

}