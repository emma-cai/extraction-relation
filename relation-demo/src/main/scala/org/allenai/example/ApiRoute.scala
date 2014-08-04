package org.allenai.example

import akka.event.LoggingAdapter
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing.HttpServiceActor

trait ApiRoute extends SprayJsonSupport { self: HttpServiceActor =>

  import context._

  object apiRoute {

    // API data transfer object
    // Note that the field name matches the 'text' field name
    // in the app-controller.js' scope.submit object.
    case class Submit(text: String)
    implicit val submitFormat = jsonFormat1(Submit.apply)

    // format: OFF
    val route =
      path("config") {
        complete("TODO")
      } ~
      path("items") {
        complete(List(1, 2, 3, 4, 5).toJson.asInstanceOf[JsArray])
      } ~
      path("submit") {
        post {
          entity(as[Submit]) { submit =>
            complete(s"[server]: You submitted '${submit.text}'")
          }
        }
      }
    // format: ON
  }
}
