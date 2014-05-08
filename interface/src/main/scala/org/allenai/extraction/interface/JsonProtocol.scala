package org.allenai.extraction.interface

import spray.json._

/** API for the Ermine service. */
object JsonProtocol extends DefaultJsonProtocol {
  /** A request to run a pipeline, sent to hostname/pipeline/{pipelineName}.
    * @param inputs a map of input name to the string to use
    */
  case class PipelineRequest(inputs: Map[String, String])
  object PipelineRequest {
    implicit val jsonFormat = jsonFormat1(PipelineRequest.apply)
  }

  case class PipelineResponse(output: String)
  object PipelineResponse {
    implicit val jsonFormat = jsonFormat1(PipelineResponse.apply)
  }
}
