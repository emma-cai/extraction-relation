package org.allenai.example

import akka.actor._
import scala.util.control.NonFatal
import spray.http.StatusCodes
import spray.http.HttpHeaders
import spray.httpx.SprayJsonSupport
import spray.routing.ExceptionHandler
import spray.routing.HttpServiceActor

object ExampleHttpServiceActor {
  def props = Props(classOf[ExampleHttpServiceActor])
}

class ExampleHttpServiceActor
    extends HttpServiceActor
    with ActorLogging
    with ApiRoute
    with SprayJsonSupport {

  override def receive = runRoute(route)

  val accessControlAllowAll =
    HttpHeaders.RawHeader("Access-Control-Allow-Origin", "http://localhost:4000")

  val accessControlAllowHeadersAll =
    HttpHeaders.RawHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")

  val exceptionHandler = ExceptionHandler {
    case NonFatal(cause) => complete(StatusCodes.InternalServerError -> cause)
  }

  // format: OFF
  val route =
    options {
      respondWithHeaders(accessControlAllowAll, accessControlAllowHeadersAll) {
        complete("")
      }
    } ~
    respondWithHeaders(accessControlAllowAll, accessControlAllowHeadersAll) {
      handleExceptions(exceptionHandler) {
        pathPrefix("api") {
          apiRoute.route
        } ~
        pathEndOrSingleSlash {
          getFromFile("public/index.html")
        } ~
        path(Rest) { rest =>
          getFromFile("public/" + rest)
        }
      }
    }
  // format: ON
}