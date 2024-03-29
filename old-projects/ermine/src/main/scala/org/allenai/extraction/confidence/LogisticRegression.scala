package org.allenai.extraction.confidence

import edu.knowitall.common.Resource.using
import edu.knowitall.tool.conf.ConfidenceFunction
import edu.knowitall.tool.conf.FeatureSet

import scala.io.Source

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.net.URL
import java.util.Scanner

/** NOTE: Will use the version in org.allenai.nlpstack.conf.impl when we connect to nlpstack
  * This version is slightly updated to allow comment lines in feature weight file (ignore lines without tab)
  */

/** An implementation of logistic regression of features that can be
  * represented as a double.
  *
  *
  * @param  featureSet  the features to use
  * @param  featureWeights  the feature weights
  * @param  intercept  the intercept value
  */
class LogisticRegression[T](
    featureSet: FeatureSet[T, Double],
    val featureWeights: Map[String, Double],
    val intercept: Double) extends ConfidenceFunction[T](featureSet) {

  featureSet.featureNames.foreach { name =>
    require(featureWeights.keySet.contains(name), "No weight for feature: " + name)
  }

  def this(featureSet: FeatureSet[T, Double], weights: Map[String, Double]) = {
    this(featureSet, weights - "Intercept", weights.getOrElse("Intercept", 0.0))
  }

  override def apply(extraction: T): Double = getConf(extraction)

  def getConf(extraction: T): Double = {

    val z = this.featureSet.featureNames.iterator.map { name =>
      val weight = featureWeights(name)
      if (weight == 0.0 || weight == -0.0) 0
      else weight * featureSet.featureMap(name).apply(extraction)
    }.sum

    1.0 / (1.0 + math.exp(-(z + this.intercept)))
  }

  override def save(output: OutputStream): Unit = {
    using(new PrintWriter(output)) { pw =>
      save(pw)
    }
  }

  def save(writer: PrintWriter): Unit = {
    for ((name, weight) <- featureWeights.toSeq.sortBy(_._1)) {
      writer.println(name + "\t" + weight)
    }

    if (!(featureWeights contains "Intercept")) {
      writer.println("Intercept" + "\t" + intercept)
    }
  }
}

object LogisticRegression {
  private val tab = """\t""".r
  def fromUrl[E](featureSet: FeatureSet[E, Double], url: URL) = {
    def buildFeatureWeightMap(input: InputStream): Map[String, Double] = {
      var featureWeights = Map.empty[String, Double]

      using(Source.fromInputStream(input)) { source =>
        source.getLines.foreach { line =>
          val parts = tab.split(line)
          if (parts.length > 1) {
            val featureName = parts(0).trim
            val weight = parts(1).toDouble
            featureWeights += featureName -> weight
          }
        }
      }

      featureWeights.toMap

    }
    using(url.openStream) { input =>
      new LogisticRegression[E](featureSet, buildFeatureWeightMap(input))
    }
  }
}