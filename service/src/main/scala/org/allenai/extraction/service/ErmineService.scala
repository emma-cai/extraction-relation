package org.allenai.extraction.service

import org.allenai.common.Logging
import org.allenai.extraction.manager.ExtractorPipeline

import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.{ Config, ConfigObject }
import spray.http.StatusCodes
import spray.httpx.SprayJsonSupport
import spray.routing.{ ExceptionHandler, HttpServiceActor }

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.control.NonFatal
          
import java.io.File
import java.io.FileWriter                                                                           
import java.io.PrintWriter                                                                          
import java.io.Writer 

class ErmineService(implicit val bindingModule: BindingModule) extends HttpServiceActor
    with Logging with SprayJsonSupport with Injectable {

  implicit def exceptionHandler = ExceptionHandler {
    case NonFatal(e) => requestUri { uri =>
      logger.warn(s"Request to ${uri} could not be handled normally: ", e)
      // TODO(jkinkead): Stack trace?
      complete(StatusCodes.InternalServerError -> "Error: " + e.getMessage)
    }
  }

  // Preload pipelines.
  val pipelineConfig = inject [Config](ServiceModule.Pipelines)
  val pipelines: Map[String,ExtractorPipeline] = {
    val mutablePipelines = for {
      (pipelineName: String, configObject: ConfigObject) <- pipelineConfig.root()
      config = configObject.toConfig
      pipeline = {
        logger.info(s"loading pipeline ${pipelineName} . . . ")
        ExtractorPipeline.fromConfig(config)
      }
    } yield (pipelineName -> pipeline)
    // Create immutable version.
    mutablePipelines.toMap
  }

  override def receive = runRoute(
    // TODO(jkinkead): Make this accept POST requests.
    pathPrefix("pipeline" / Segment / """(.*)""".r) { (name, text) =>
      pipelines.get(name) match {
        case Some(pipeline) => {
          logger.info("running pipeline '" + name + "' . . .")

          // TODO(jkinkead): Create infile(s).
          val input = Source.fromString(text)
          val output = new java.io.StringWriter()
          pipeline.run(input, output)
          output.close

          complete("extractions:\n" + output.toString())
        }
        case None => complete(StatusCodes.BadRequest -> s"No pipeline with name '${name}' exists")
      }
    }
  )
}
