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
    val processed: Seq[Future[(ExtractedSentence, Seq[Throwable])]] = for (sentence <- sentences) yield {
      logger.debug(s"Processing sentence with ${extractors.size} extractors: " + sentence)

      type Extractor = String
      type Extraction = String

      // Fire off requests to all extractors for the particular sentence.
      // We use a Try so that we can keep all failures, instead of failing
      // the whole future for a single failure.
      val extractionsFuture: Seq[Future[Try[(Extractor, Extraction)]]] =
        for (extractor <- extractors) yield {
          // Run the extractor
          val attempt: Future[Extraction] = extractor(sentence)

          // Wrap with a cleaner exception
          val wrapped: Future[Extraction] = attempt.transform(x => x, throwable =>
            new ExtractorException(
              s"Exception with ${extractor.name} at: ${extractor.url}", throwable))

          // Convert future value to Try.
          wrapped map { extractions =>
            Success((extractor.name, extractions))
          } recover {
            case NonFatal(e) =>
              logger.error(e.getMessage, e)
              Failure(e)
          }
        }

      // Change responses into an ExtractedSentence and failures.
      Future.sequence(extractionsFuture) map { seq: Seq[Try[(String, String)]] =>
        val successfulExtractions = for {
          tryValue <- seq
          success <- tryValue.toOption
          (extractor, response) = success
          extractions = (response split "\n")
        } yield ExtractorResults(extractor, extractions)

        val exceptions = seq collect { case fail: Failure[_] => fail.exception }

        (ExtractedSentence(sentence, successfulExtractions), exceptions)
      }
    }

    val extractorMap = extractors.map { extractor: Extractor =>
      extractor.name -> extractor.description.getOrElse("")
    }.toMap

    Future.sequence(processed) map { seq =>
      val successes: Seq[ExtractedSentence] = seq map (_._1)
      val exceptions: Seq[Throwable] = seq flatMap (_._2)
      Response(successes, extractorMap, exceptions)
    }
  }

  case class Response(sentences: Seq[ExtractedSentence], extractors: Map[String, String], failures: Seq[Throwable])
  case class ExtractedSentence(text: String, extractors: Seq[ExtractorResults])
  case class ExtractorResults(name: String, extractions: Seq[String])

  implicit val throwableWriter = new RootJsonFormat[Throwable] {
    def read(v: JsValue): Throwable = throw new UnsupportedOperationException
    /** Write a throwable as an object with 'message' and 'stackTrace' fields. */
    def write(t: Throwable) = {
      def getMessageChain(throwable: Throwable): List[String] = {
        Option(throwable) match {
          case Some(throwable) => Option(throwable.getMessage) match {
            case Some(message) => (throwable.getClass + ": " + message) :: getMessageChain(throwable.getCause)
            case None => getMessageChain(throwable.getCause)
          }
          case None => Nil
        }
      }
      val stackTrace = {
        val stackTraceWriter = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(stackTraceWriter))
        stackTraceWriter.toString
      }
      JsObject(
        "messages" -> JsArray(getMessageChain(t) map (JsString(_))),
        "stackTrace" -> JsString(stackTrace))
    }
  }
  implicit val extractorResults = jsonFormat2(ExtractorResults)
  implicit val extractedSentenceFormat = jsonFormat2(ExtractedSentence)
  implicit val responseFormat = jsonFormat3(Response)

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

    implicit def exceptionHandler(implicit log: spray.util.LoggingContext) =
        ExceptionHandler {
          case e: Throwable => ctx =>
            // log in akka, which is configured to use slf4j
            log.error(e, "Unexpected Error.")

            // return the error formatted as json
            ctx.complete((InternalServerError, e.toJson.prettyPrint))
        }

    startServer(interface = "0.0.0.0", port = port) {
      handleExceptions(exceptionHandler) {
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
}

class ExtractorException(message: String, cause: Throwable) extends Exception(message, cause)

case class Extractor(url: URL) extends Logging {
  override def toString = s"$name($url)"

  val name: String = {
    val svc = dispatch.url(url.toString) / "info" / "name"
    try {
      Await.result(Http(svc OK as.String), 10.seconds).trim
    }
    catch {
      case NonFatal(e) =>
        logger.error("Could not access /info/name at: " + url.toString, e)
        throw e
    }
  }

  val description: Option[String] = {
    val svc = dispatch.url(url.toString) / "info" / "description"
    Try(Await.result(Http(svc OK as.String), 10.seconds)).toOption map (_.trim)
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
