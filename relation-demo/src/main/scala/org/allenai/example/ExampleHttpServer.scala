package org.allenai.example

import org.allenai.common.Config._

import akka.actor._
import akka.io.IO
import com.typesafe.config.ConfigFactory
import spray.can.Http

/** ExampleHttpServer is the main application that bootstraps the HttpServer */
object ExampleHttpServer extends App {

  implicit val system: ActorSystem = ActorSystem("example-server")

  val config = ConfigFactory.load()

  // The ProductionContext provides an implicit ActorSystem as a field named `system`
  val service = system.actorOf(ExampleHttpServiceActor.props, "example")

  // Set up our service actor to listen for HTTP requests on the configured port
  IO(Http) ! Http.Bind(service, "0.0.0.0", port = config[Int]("org.allenai.example.port"))
}
