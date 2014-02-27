package org.allenai.extraction.demo

import org.allenai.common.Resource.using
import org.allenai.common.Logging

import akka.actor._
import com.typesafe.config.ConfigFactory
import dispatch.{ Future => _, _ }
import dispatch.Defaults._
import spray.http._
import spray.http.StatusCodes._
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.routing._
import de.l3s.boilerpipe.extractors.ArticleExtractor

import java.io.File
import java.net.URL
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{ Try, Success, Failure }

class ExtractionDemo(extractors: Seq[Extractor])(port: Int) extends SimpleRoutingApp with SprayJsonSupport with Logging {
  val timeout = 1.minute

  def extractSentences(sentences: Iterator[String]): String = {
    val processed = for (sentence <- sentences) yield {
      logger.debug(s"Processing sentence with ${extractors.size} extractors: " + sentence)

      // Fire off requests to extractors.
      val extractionsFuture: Seq[Future[(String, String)]] = 
        for (extractor <- extractors) yield {
          val future = extractor(sentence)
          future.map((extractor.name, _))
        }

      // Wait for the results.
      val extractions: Seq[(String, String)] = 
        Await.result(Future.sequence(extractionsFuture), timeout)
        
      // Split the responses into multiple extractions.
      extractions map { case (extractor, response) =>
        (extractor, (response split "\n").toSeq)
      }
    }
    
    processed mkString "\n"
  }

  def extractSentences(sentences: Seq[String]): String = {
    extractSentences(sentences.iterator)
  }

  def run() {
    val staticContentRoot = "public"

    val cacheControlMaxAge = HttpHeaders.`Cache-Control`(CacheDirectives.`max-age`(60))

    implicit val system = ActorSystem("extraction-demo")

    startServer(interface = "0.0.0.0", port = port) {
      respondWithHeader(cacheControlMaxAge) {
        path ("") {
          get {
            getFromFile(staticContentRoot + "/index.html")
          }
        } ~
        post {
          path ("text") {
            formFields("text") { text =>
              val sentences = text split "\n"
              val sampleText = (sentences.headOption map (s => (s take 32) + "...")).getOrElse("")
              logger.info(s"Request to extract ${sentences.size} sentences: " + sampleText)
              complete {
                extractSentences(text split "\n")
              }
            }
          } ~
          path ("file") {
            formFields("file") { fileString =>
              val file = new File(fileString)
              logger.info("Request to extract file: " + file)
              using(Source.fromFile(file)) { source =>
                complete {
                  extractSentences(source.getLines)
                }
              }
            }
          } ~
          path ("url") {
            formFields("url") { urlString =>
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

case class Extractor(url: URL) {
  override def toString = s"$name($url)"

  val name: String = {
    val svc = dispatch.url(url.toString) / "info" / "name"
    Await.result(Http(svc OK as.String), 10.seconds)
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
