package org.allenai.extraction.confidence

import org.allenai.extraction.processors.definition.OtterNounDefinitionExtractor
import org.allenai.extraction.processors.definition.OtterToken

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import edu.knowitall.common.Resource
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.control.Exception

import java.io.File

/* Helper function for dumping definition extractions and features
 * for use in confidence function tagging and training.
 * It currently reads raw textual preprocessor output. 
 * TODO: Update to read JSON preprocessor output.
 */
object DumpExtractionsAndFeatures {
  val logger = LoggerFactory.getLogger(this.getClass)

  case class Settings(
    var inputFile: File = null,
    /* TODO: Make a proper outputFile option */
    //var outputFile: Option[File] = None,
    var count: Int = Int.MaxValue,
    var includeFeatures: Boolean = false,
    var includeScores: Boolean = false)

  def main(args: Array[String]) {

    val parser = new scopt.OptionParser[Settings]("scoreextr") {
      arg[String]("gold") text ("gold set") action { (path: String, config) => config.copy(inputFile = new File(path)) }
      //opt[String]('o', "output") text("output file") action { (path: String, config: Settings) => config.copy(outputFile = Some(new File(path))) }
      opt[String]('f', "features") action { (f: String, config) => config.copy(includeFeatures = (f.matches("true|1"))) }
      opt[String]('s', "scores") action { (f: String, config) => config.copy(includeScores = (f.matches("true|1"))) }
      opt[Int]('c', "count") text ("number of sentences to use") action { (i: Int, config) => config.copy(count = i) }
    }

    parser.parse(args, Settings()) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def preprocessLine(defnInputLine: String): (String, String, String) = {
    defnInputLine.split("\t").toList match {
      // For handling output format from Preprocessor
      case List(term, termWordClass, termDefinition, _*) => (term.trim, termWordClass.trim, termDefinition.trim)
      // For handling just the raw definition- for e.g., when using the ermine service through the web demo
      case Nil :+ termDefinition => ("", "", termDefinition.trim)
      case _ => ("", "", "")
    }
  }

  def run(settings: Settings) {

    val conf = ExtractionConfidenceFunction.loadDefaultOtterClassifier()
    val config = ConfigFactory.load()
    val dataDirectory = "/Users/tafjord/gitroot/extraction/ermine/src/main/data/definitions"

    println("dataDirectory = " + dataDirectory)

    val myOtterExtractor = new OtterNounDefinitionExtractor(dataDirectory)

    logger.info("Reading input from " + settings.inputFile + "...")
    val input =
      Resource.using(Source.fromFile(settings.inputFile, "UTF8")) { source => source.getLines.toList.take(settings.count) }

    println("*** FEATURES ***")
    if (settings.includeFeatures) OtterFeatures.featureMap.foreach { case (name, feature) => println("::: " + name) }

    println("*** EXTRACTIONS ***")

    for (line <- input) {
      println(";;; DEFINITION:   " + line)

      val (term, termWordClass, termDefinition) = preprocessLine(line)

      val (extrs, tokens) = myOtterExtractor.extract(term, termDefinition)

      val otterTokens = OtterToken.makeTokenSeq(tokens)

      for (extr <- extrs; extrAnno = OtterExtractionTupleAnnotated(extr, otterTokens)) {
        println(
          ";;; " + extr.toSimpleString + "\t" +
            (if (!settings.includeScores) "" else {
              conf(extr.relation.relationType.toString)(extrAnno) + "\t"
            }) +
            otterTokens.map(_.toFullString).mkString(" ") +
            (if (!settings.includeFeatures) "" else {
              val features = OtterFeatures.featureMap.map { case (name, feature) => feature(extrAnno) }
              "\t" + features.mkString("\t")
            }))
      }
    }

  }
}
