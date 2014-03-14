package org.allenai.extraction.demo

import org.allenai.common.Resource.using
import org.allenai.common.Logging

import akka.actor._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigRenderOptions
import de.l3s.boilerpipe.extractors.ArticleExtractor
import dispatch.Defaults._
import dispatch.{ Future => _, _ }
import spray.http.StatusCodes._
import spray.http._
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol._
import spray.json._
import spray.routing._

import java.io.File
import java.net.URL
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{ Try, Success, Failure }
import scala.util.control.NonFatal

class ExtractionDemo(extractors: Seq[Extractor])(port: Int) extends SimpleRoutingApp with SprayJsonSupport with Logging {
  val timeout = 1.minute

  def extractSentences(sentences: Seq[String]): Future[Response] = {
    val processed = for (sentence <- sentences) yield {
      logger.debug(s"Processing sentence with ${extractors.size} extractors: " + sentence)

      // Fire off requests to extractors.
      val extractionsFuture: Seq[Future[(String, String)]] =
        for (extractor <- extractors) yield {
          val future = extractor(sentence)
          future.map((extractor.name, _))
        }

      Future.sequence(extractionsFuture) map { f =>
        val extractorResults = for {
          (extractor, response) <- f
          extractions = (response split "\n")
        } yield ExtractorResults(extractor, extractions)

        ExtractedSentence(sentence, extractorResults)
      }
    }

    Future.sequence(processed) map (Response(_))
  }

  case class Response(sentences: Seq[ExtractedSentence])
  case class ExtractedSentence(text: String, extractors: Seq[ExtractorResults])
  case class ExtractorResults(name: String, extractions: Seq[String])

  implicit val extractorResults = jsonFormat2(ExtractorResults)
  implicit val extractedSentenceFormat = jsonFormat2(ExtractedSentence)
  implicit val responseFormat = jsonFormat1(Response)

  def run() {
    val config = ConfigFactory.load().getConfig("extraction")

    val staticContentRoot = "public"
    val staticContentRootFile = new File(staticContentRoot)
    val defaultContent = "index.html"
    val defaultContentRootFile = new File(staticContentRootFile, defaultContent)

    require(staticContentRootFile.exists, "Static content root not found: " + staticContentRootFile.getAbsolutePath)
    require(defaultContentRootFile.exists, "Static default content not found: " + defaultContentRootFile.getAbsolutePath)

    val cacheControlMaxAge = HttpHeaders.`Cache-Control`(CacheDirectives.`max-age`(60))

    implicit val system = ActorSystem("extraction-demo")

    startServer(interface = "0.0.0.0", port = port) {
      respondWithHeader(cacheControlMaxAge) {
        path ("") {
          get {
            getFromFile(staticContentRoot + "/index.html")
          }
        } ~
        path("config") {
          get {
            val asJsonOptions = ConfigRenderOptions.defaults().setJson(true)
            complete(config.root.render(asJsonOptions))
          }
        } ~
        post {
          path ("text") {
            entity(as[String]) { text =>
              val sentences = text split "\n"
              val sampleText = (sentences.headOption map (s => (s take 32) + "...")).getOrElse("")
              logger.info(s"Request to extract ${sentences.size} sentences: " + sampleText)
              complete {
                extractSentences(text split "\n")
              }
            }
          } ~
          path ("file") {
            entity(as[String]) { fileString =>
              val file = new File(fileString)
              logger.info("Request to extract file: " + file)
              using(Source.fromFile(file)) { source =>
                complete {
                  extractSentences(source.getLines.toSeq)
                }
              }
            }
          } ~
          path ("url") {
            entity(as[String]) { urlString =>
              logger.info("Request to extract url: " + urlString)
              val url = new URL(urlString)
              val text = ArticleExtractor.INSTANCE.getText(url)
              complete {
                extractSentences(text split "\n")
              }
            }
          }
        } ~
        unmatchedPath { p => getFromFile(staticContentRoot + p) }
      }
    }
  }
}

case class Extractor(url: URL) extends Logging {
  override def toString = s"$name($url)"

  val name: String = {
    val svc = dispatch.url(url.toString) / "info" / "name"
    try {
      Await.result(Http(svc OK as.String), 10.seconds)
    }
    catch {
      case NonFatal(e) =>
        logger.error("Could not access /info/name at: " + url.toString, e)
        throw e
    }
  }

  def apply(sentence: String): Future[String] = {
    val svc = dispatch.url(url.toString) << sentence
    Http(svc OK as.String)
  }
}

object ExtractionDemoMain extends App with Logging {
  val config = ConfigFactory.load().getConfig("extraction.demo")

  val port = config.getInt("port")
  val extractors = config.getStringList("extractors").iterator().asScala.map { url =>
      new Extractor(new URL(url))
    }.toSeq

  logger.info("Configured with extractors: " + extractors.mkString(", "))

  val server = new ExtractionDemo(extractors)(config.getInt("port"))
  server.run()
}
