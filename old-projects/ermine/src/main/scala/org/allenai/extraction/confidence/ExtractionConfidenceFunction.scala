package org.allenai.extraction.confidence

import org.allenai.common.Logging

import edu.knowitall.tool.conf.FeatureSet
import org.slf4j.LoggerFactory

import java.net.URL

/* Set up confidence function for scoring extractions. Currently specialized to OtterExtractions,
 * will be generalized when other cases are available.
 * 
 * Current call pattern given an OtterExtractionTuple (oet) inside an OtterExtractionForDefinitionAlternate (oefda)
 * is a bit cumbersome, will be streamlined (TODO):
 * val conf = ExtractionConfidenceFunction.loadDefaultOtterClassifier()
 * val score = conf(oet.relation.relationType.toString)(OtterExtractionTupleAnnotated(oet, oefda.preprocessedDefinitionTokens)
 */
object ExtractionConfidenceFunction extends Logging {

  type ExtractionConfidenceFunction = LogisticRegression[OtterExtractionTupleAnnotated]

  def defaultModelUrl() = Option(this.getClass.getResource("default-classifier.txt")).getOrElse {
    throw new IllegalArgumentException("Could not load confidence function resource.")
  }

  def getOtterModelUrl(c: String) = {
    val fname = s"otter-fweights-$c.txt"
    Option(this.getClass.getResource(fname)).getOrElse {
      throw new IllegalArgumentException(s"Could not load confidence function resource: $fname")
    }
  }

  val otterClassifierCases = List("isa")
  def loadDefaultOtterClassifier(): Map[String, ExtractionConfidenceFunction] = {
    val list1 = otterClassifierCases map (c => (c, fromUrl(OtterFeatureSet, getOtterModelUrl(c))))
    list1.toMap withDefaultValue fromUrl(OtterFeatureSet, getOtterModelUrl("default"))
  }

  def fromUrl(featureSet: FeatureSet[OtterExtractionTupleAnnotated, Double], url: URL): ExtractionConfidenceFunction = {
    LogisticRegression.fromUrl(featureSet, url)
  }
}