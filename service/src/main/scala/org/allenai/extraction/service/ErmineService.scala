package org.allenai.extraction.service

import org.allenai.extraction.api.JsonProtocol.{ PipelineRequest, PipelineResponse }
import org.allenai.extraction.manager.ErminePipeline

import akka.actor.{ ActorLogging, Props }
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.{ Config, ConfigObject }
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport
import spray.routing.{ ExceptionHandler, HttpServiceActor }

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.control.NonFatal

import java.io.StringWriter

class ErmineService(implicit val bindingModule: BindingModule) extends HttpServiceActor
    with ActorLogging with SprayJsonSupport with Injectable {

  implicit def exceptionHandler = ExceptionHandler {
    case NonFatal(e) => requestUri { uri =>
      log.error(e, s"Request to ${uri} could not be handled normally:")
      // TODO(jkinkead): Stack trace?
      complete(StatusCodes.InternalServerError -> "Error: " + e.getMessage)
    }
  }

  // Preload pipelines.
  val pipelineConfig = inject [Config](ServiceModuleId.Pipelines)
  val pipelines: Map[String,ErminePipeline] = {
    val mutablePipelines = for {
      (pipelineName: String, configObject: ConfigObject) <- pipelineConfig.root()
      config = configObject.toConfig
      pipeline = {
        log.info(s"loading pipeline ${pipelineName} . . . ")
        ErminePipeline.fromConfig(config)
      }
    } yield (pipelineName -> pipeline)
    // Create immutable version.
    mutablePipelines.toMap
  }

  log.info("done loading pipelines, ready for requests!")

  // format: OFF
  override def receive = runRoute(
    pathPrefix("pipeline" / Segment) { pipelineName =>
      pipelines.get(pipelineName) match {
        case Some(pipeline) => {
          pathEnd {
            post {
              entity(as[PipelineRequest]) { pipelineRequest =>
                log.info(s"running pipeline '${pipelineName}' . . .")

                val inputs = for {
                  (name, text) <- pipelineRequest.inputs
                } yield (name -> Source.fromString(text))
                val output = new StringWriter()
                // TODO(jkinkead): Allow for multiple named outputs?
                pipeline.run(inputs, Seq.empty, Seq(output))
                output.close

                log.info(s"pipeline '${pipelineName}' complete.")
                complete(PipelineResponse(output.toString))
              }
            }
          } ~
          path("info" / "name" ) {
            complete(pipeline.name)
          } ~
          path("info" / "description" ) {
            complete(pipeline.description)
          }
        }
        case None => complete(StatusCodes.BadRequest ->
            s"No pipeline with name '${pipelineName}' exists")
      }
    }
  )
  // format: ON
}

object ErmineService {
  def props(bindingModule: BindingModule): Props = Props(classOf[ErmineService], bindingModule)
}
