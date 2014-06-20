package org.allenai.extraction.demo.otter

import org.allenai.common.Resource.using
import org.allenai.common.Logging
import org.allenai.extraction.api.JsonProtocol.{ PipelineRequest, PipelineResponse }
import org.allenai.extraction.demo._

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

class OtterExtractionDemo(extractor: ErmineProcessor, dbProcessor: ErmineProcessor)(port: Int) extends SimpleRoutingApp with SprayJsonSupport with Logging {
  val timeout = 1.minute

  def processDefinitionsOrSearchTerms(inputs: Seq[String], processor: ErmineProcessor): Future[Response] = {
    val processed: Seq[Future[(ProcessedInput, Seq[Throwable])]] = for (input <- inputs) yield {
      logger.debug(s"Processing input: " + input)

      // Fire off request to the processor for the particular input- this
      // is either search terms or definitions.
      // We use a Try so that we can keep all failures, instead of failing
      // the whole future for a single failure.
      val processingFutures: Seq[Future[Try[ProcessorResponse]]] = {
        // Run the extractor
        val attempt: Future[String] = processor(input)

        // Wrap with a cleaner exception
        val wrapped: Future[String] = attempt.transform(x => x, throwable =>
          new ProcessorException(
            s"Exception with ${processor.name} at: ${processor.url}", throwable))

        // Convert future value to Try.
        val f = wrapped map { extractions =>
          Success(ProcessorResponse(processor.name, extractions))
        } recover {
          case NonFatal(e) =>
            logger.error(e.getMessage, e)
            Failure(e)
        }
        Seq[Future[Try[ProcessorResponse]]](f)
      }

      // Change responses into an ProcessedInput and failures.
      Future.sequence(processingFutures) map { seq: Seq[Try[ProcessorResponse]] =>
        val successfulExtractions = for {
          tryValue <- seq
          success <- tryValue.toOption
          ProcessorResponse(extractor, response) = success
          extractions = (response split "\n")
        } yield ProcessorResults(extractor, extractions)

        for (p <- successfulExtractions) {
          for (e <- p.extractions) {
            logger.info("extractions: " + e)
          }
        }

        val exceptions = seq collect { case fail: Failure[_] => fail.exception }

        (ProcessedInput(input, successfulExtractions), exceptions)
      }
    }

    val extractorMap = Seq[Processor](extractor, dbProcessor).map { extractor: Processor =>
      extractor.name -> extractor.description.getOrElse("")
    }.toMap

    Future.sequence(processed) map { seq =>
      val successes: Seq[ProcessedInput] = seq map (_._1)
      val exceptions: Seq[Throwable] = seq flatMap (_._2)
      Response(successes, extractorMap, exceptions)
    }
  }

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

  implicit val processorResults = jsonFormat2(ProcessorResults)
  implicit val processedInputFormat = jsonFormat2(ProcessedInput)
  implicit val responseFormat = jsonFormat3(Response)

  def run() {
    val config = ConfigFactory.load().getConfig("extraction")

    val staticContentRoot = "public"
    val staticContentRootFile = new File(staticContentRoot)
    val defaultContent = "otter-index.html"
    val defaultContentRootFile = new File(staticContentRootFile, defaultContent)

    logger.info("defaultContentRootFile: " + defaultContentRootFile)
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

    // format: OFF
    startServer(interface = "0.0.0.0", port = port) {
      handleExceptions(exceptionHandler) {
        respondWithHeader(cacheControlMaxAge) {
          path ("") {
            get {
              getFromFile(defaultContentRootFile)
            }
          } ~
          path("config") {
            get {
              val asJsonOptions = ConfigRenderOptions.defaults().setJson(true)
              complete(config.root.render(asJsonOptions))
            }
          } ~
          post {
            path ("definition") {
              entity(as[String]) { text =>
                val definitions = text split "\n"
                val sampleText = (definitions.headOption map (s => (s take 32) + "...")).getOrElse("")
                logger.info(s"Request to extract ${definitions.size} definition(s): " + sampleText)
                complete {
                  processDefinitionsOrSearchTerms(text split "\n", extractor)
                }
              }
            } ~
            path ("searchterm") {
              entity(as[String]) { text =>
                val searchTerms = text split "\n"
                val sampleText = searchTerms.headOption getOrElse("")
                logger.info(s"Request to look up extractions for ${searchTerms.size} search term(s): " + sampleText + ",...")
                complete {
                  processDefinitionsOrSearchTerms(text split "\n", dbProcessor)
                }
              }
            }
          } ~
          unmatchedPath { p => getFromFile(staticContentRoot + p) }
        }
      }
    }
    // format: ON
  }
}

object OtterExtractionDemoMain extends App with Logging {
  val config = ConfigFactory.load().getConfig("otter.extraction.demo")

  val port = config.getInt("port")
  val extractor = new ErmineProcessor(new URL(config.getString("extractor")))
  val dbProcessor = new ErmineProcessor(new URL(config.getString("db-processor")))

  logger.info("Configured processors: " + "(" + extractor + ", " + dbProcessor + ")")
  val server = new OtterExtractionDemo(extractor, dbProcessor)(config.getInt("port"))
  server.run()
}