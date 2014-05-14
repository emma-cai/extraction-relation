package org.allenai.extraction.service

import org.allenai.extraction.interface.JsonProtocol.{ PipelineRequest, PipelineResponse }
import org.allenai.extraction.manager.ExtractorPipeline

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
  val pipelines: Map[String,ExtractorPipeline] = {
    val mutablePipelines = for {
      (pipelineName: String, configObject: ConfigObject) <- pipelineConfig.root()
      config = configObject.toConfig
      pipeline = {
        log.info(s"loading pipeline ${pipelineName} . . . ")
        ExtractorPipeline.fromConfig(config)
      }
    } yield (pipelineName -> pipeline)
    // Create immutable version.
    mutablePipelines.toMap
  }

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
                pipeline.run(inputs, Seq.empty, output)
                output.close

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
