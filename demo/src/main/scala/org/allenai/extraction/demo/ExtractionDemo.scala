package org.allenai.extraction.demo

import org.allenai.common.Resource.using
import org.allenai.common.Logging
import org.allenai.extraction.api.JsonProtocol.{ PipelineRequest, PipelineResponse }

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

/** Helper Object for reading/writing exception messages.
  */
object ThrowableWriter {
  implicit val ThrowableWriterRootJsonFormat = new RootJsonFormat[Throwable] {
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
}

/** Class to handle various routes for the General demo and Otter demo.
  * @param staticContentRoot Path of the directory with the required html and related resources.
  * @param defaultContent Home page for the demo: different values for the General and the Otter demos.
  * Defaults to the one for the General demo.
  */
class ExtractionDemo(extractors: Seq[Processor],
    val staticContentRoot: String = "public",
    val defaultContent: String = "index.html") extends Directives with SprayJsonSupport with Logging {

  val config = ConfigFactory.load().getConfig("extraction")
  val staticContentRootFile = new File(staticContentRoot)
  val defaultContentRootFile = new File(staticContentRootFile, defaultContent)

  require(staticContentRootFile.exists, "Static content root not found: " + staticContentRootFile.getAbsolutePath)
  require(defaultContentRootFile.exists, "Static default content not found: " + defaultContentRootFile.getAbsolutePath)

  import org.allenai.extraction.demo.ThrowableWriter.ThrowableWriterRootJsonFormat
  val timeout = 1.minute

  def processInput(input: Seq[String]): Future[Response] = {
    val processed: Seq[Future[(ProcessedInput, Seq[Throwable])]] = for (inputLine <- input) yield {
      logger.debug(s"Processing input with ${extractors.size} extractors: " + inputLine)

      case class ProcessorResponse(extractor: String, response: String)

      // Fire off requests to all extractors for the particular sentence.
      // We use a Try so that we can keep all failures, instead of failing
      // the whole future for a single failure.
      val extractionFutures: Seq[Future[Try[ProcessorResponse]]] =
        for (extractor <- extractors) yield {
          // Run the extractor
          val attempt: Future[String] = extractor(inputLine)

          // Wrap with a cleaner exception
          val wrapped: Future[String] = attempt.transform(x => x, throwable =>
            new ProcessorException(
              s"Exception with ${extractor.name} at: ${extractor.url}", throwable))

          // Convert future value to Try.
          wrapped map { extractions =>
            Success(ProcessorResponse(extractor.name, extractions))
          } recover {
            case NonFatal(e) =>
              logger.error(e.getMessage, e)
              Failure(e)
          }
        }

      // Change responses into an ProcessedInput and failures.
      Future.sequence(extractionFutures) map { seq: Seq[Try[ProcessorResponse]] =>
        val successfulExtractions = for {
          tryValue <- seq
          success <- tryValue.toOption
          ProcessorResponse(extractor, response) = success
          extractions = (response split "\n")
        } yield ProcessorResults(extractor, extractions)

        val exceptions = seq collect { case fail: Failure[_] => fail.exception }

        (ProcessedInput(inputLine, successfulExtractions), exceptions)
      }
    }

    val extractorMap = extractors.map { extractor: Processor =>
      extractor.name -> extractor.description.getOrElse("")
    }.toMap

    Future.sequence(processed) map { seq =>
      val successes: Seq[ProcessedInput] = seq map (_._1)
      val exceptions: Seq[Throwable] = seq flatMap (_._2)
      Response(successes, extractorMap, exceptions)
    }
  }

  case class Response(sentences: Seq[ProcessedInput], extractors: Map[String, String], failures: Seq[Throwable])
  case class ProcessedInput(text: String, extractors: Seq[ProcessorResults])
  case class ProcessorResults(name: String, extractions: Seq[String])

  implicit val extractorResults = jsonFormat2(ProcessorResults)
  implicit val extractedSentenceFormat = jsonFormat2(ProcessedInput)
  implicit val responseFormat = jsonFormat3(Response)

  implicit val actorSystem = ActorSystem("extraction-demo")

  // Index route: Handles the home page url, i.e., without any path prefixes.
  val indexRoute =
    get {
      logger.info(s"Request for main page")
      getFromFile(defaultContentRootFile)
    }

  // Route: Handles all possible paths that can follow the urls for the General demo
  // and Otter demo (/general/... and /otter/... respectively).
  val route =
    path("config") {
      get {
        val asJsonOptions = ConfigRenderOptions.defaults().setJson(true)
        complete(config.root.render(asJsonOptions))
      }
    } ~
      post {
        path("text") {
          entity(as[String]) { text =>
            val sentences = text split "\n"
            val sampleText = (sentences.headOption map (s => (s take 32) + "...")).getOrElse("")
            logger.info(s"Request to extract ${sentences.size} sentences: " + sampleText)
            complete {
              processInput(text split "\n")
            }
          }
        } ~
          path("file") {
            entity(as[String]) { fileString =>
              val file = new File(fileString)
              logger.info("Request to extract file: " + file)
              using(Source.fromFile(file)) { source =>
                complete {
                  processInput(source.getLines.toSeq)
                }
              }
            }
          } ~
          path("url") {
            entity(as[String]) { urlString =>
              logger.info("Request to extract url: " + urlString)
              val url = new URL(urlString)
              val text = ArticleExtractor.INSTANCE.getText(url)
              complete {
                processInput(text split "\n")
              }
            }
          }
      }
}

class ProcessorException(message: String, cause: Throwable = null) extends Exception(message, cause)

/** Abstract class for a processor, which can be an old style processor or an Ermine processor.
  * @param url Path of the processor we are requesting extraction/other processing from. These come
  * from the config file.
  */
abstract class Processor(val url: URL) extends (String => Future[String]) with Logging {
  override def toString = s"$name($url)"

  val name: String = {
    val svc = dispatch.url(url.toString) / "info" / "name"
    try {
      Await.result(Http(svc OK as.String), 10.seconds).trim
    } catch {
      case NonFatal(e) =>
        logger.error("Could not access /info/name at: " + url.toString, e)
        throw e
    }
  }

  val description: Option[String] = {
    val svc = dispatch.url(url.toString) / "info" / "description"
    Try(Await.result(Http(svc OK as.String), 10.seconds)).toOption map (_.trim)
  }
}

/** Old-style extractor which accepts a POST on the root path and returns a string.
  */
class SimpleProcessor(url: URL) extends Processor(url) {
  override def apply(sentence: String): Future[String] = {
    val svc = dispatch.url(url.toString) << sentence
    Http(svc OK as.String)
  }
}

/** Extractor run through the Ermine service.
  */
class ErmineProcessor(url: URL) extends Processor(url) {
  override def apply(sentence: String): Future[String] = {
    // TODO(jkinkead): This is hard-coded to use a "text" input, which doesn't work for the
    // ferret-question extractor. We should figure out a way to specify input streams in the config
    // file . . . and add that to the UI.
    val request = PipelineRequest(Map("text" -> sentence))
    val svc = dispatch.url(url.toString).addHeader("Content-Type", "application/json") <<
      request.toJson.compactPrint
    Http(svc) map { response =>
      response.getStatusCode match {
        case 200 =>
          val body = response.getResponseBody
          body.parseJson.convertTo[PipelineResponse].output
        case responseCode =>
          val body = response.getResponseBody
          throw new ProcessorException(s"Bad response ($responseCode) from Ermine at $url: $body")
      }
    }
  }
}

/** The main app class. Since we now have both the (old) General demo and the (new) Otter demo,
  * this takes both objects and composes the overall routes using the routes defined within each
  * of the demo objects.
  * @param generalDemo ExtractionDemo object representing the "general" extraction demo: old demo.
  * @param otterDemo ExtractionDemo object representing the new Otter extraction demo.
  */
class ExtractionDemoApp(
    generalDemo: ExtractionDemo,
    otterDemo: ExtractionDemo,
    staticContentRoot: String = "public")(port: Int) extends SimpleRoutingApp with SprayJsonSupport with Logging {
  import org.allenai.extraction.demo.ThrowableWriter.ThrowableWriterRootJsonFormat

  def run() {
    val cacheControlMaxAge = HttpHeaders.`Cache-Control`(CacheDirectives.`max-age`(60))

    implicit def exceptionHandler(implicit log: spray.util.LoggingContext) =
      ExceptionHandler {
        case e: Throwable => ctx =>
          // log in akka, which is configured to use slf4j
          log.error(e, "Unexpected Error.")

          // return the error formatted as json
          ctx.complete((InternalServerError, e.toJson.prettyPrint))
      }

    implicit val actorSystem = ActorSystem("extraction-demo")
        // format: OFF
        startServer(interface = "0.0.0.0", port = port) {
          handleExceptions(exceptionHandler) {
            respondWithHeader(cacheControlMaxAge) {
              // Go to General demo home page if there is no additional path prefix, i.e., 
              // it is empty or has a '/'.
              pathEndOrSingleSlash {
                generalDemo.indexRoute 
              } ~
              // Go to General demo if the path has a '/general' prefix specified -
              // so there are two ways to get to the General extraction demo: one is by
              // using '/general' and the other is by not specifying a prefix (as above).
              pathPrefix("general") {
                pathEndOrSingleSlash {
                  generalDemo.indexRoute
                } ~
                generalDemo.route 
              } ~
              // Go to Otter demo if the path has a '/otter' prefix specified.
              pathPrefix("otter") {
                pathEndOrSingleSlash {
                  otterDemo.indexRoute
                } ~
                otterDemo.route 
              } ~             
              unmatchedPath { p => getFromFile(staticContentRoot + p) }
            }
          }
       }
    }
}
  
/** Companion object to above ExtractionDemoApp class. Creates the General demo and Otter demo
  * objects based on their specific config information from the application config. The required
  * processors for each demo are created and each demo is initialized with its set of processors.
  */
object ExtractionDemoApp extends App with Logging {
  
  /** Create and initialize the demo object with the required set of processors (as obtained from
    * the application config file).
    * @param staticContentRoot path of the html resources.
    * @param defaultConent home page for the demo.
    */
  def initializeExtractionDemo(configKey: String,
    staticContentRoot: String = "public",
    defaultContent: String) : ExtractionDemo = {
    val config = ConfigFactory.load().getConfig(configKey)

    val simpleExtractors = config.getStringList("extractors").iterator().asScala.map { url =>
      new SimpleProcessor(new URL(url))
    }.toSeq
    val ermineProcessors = config.getStringList("ermine-extractors").iterator().asScala.map { url =>
      new ErmineProcessor(new URL(url))
    }.toSeq

    val processors = simpleExtractors ++ ermineProcessors

    logger.info("Configured with extractors: " + processors.mkString(", "))

    new ExtractionDemo(processors, staticContentRoot, defaultContent)
  }

  // Read the application conf file, get the config info for the General and Otter demos, create
  // and initialize those demo objects. This App object then contains both those demo objects.
  val config = ConfigFactory.load().getConfig("extraction.demo")
  val port = config.getInt("port")
  val generalDemo = initializeExtractionDemo("general.extraction.demo", defaultContent = "index.html")
  val otterDemo = initializeExtractionDemo("otter.extraction.demo", defaultContent = "otter.html")
  val server = new ExtractionDemoApp(generalDemo, otterDemo) (config.getInt("port"))
  server.run
}
