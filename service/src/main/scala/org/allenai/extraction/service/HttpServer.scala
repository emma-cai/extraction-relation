package org.allenai.extraction.service

import akka.actor.{ ActorSystem, Props }
import akka.io.IO
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import com.typesafe.config.ConfigFactory
import spray.can.Http

class HttpServer(implicit val bindingModule: BindingModule) extends Injectable {
  val port = inject [Int](ServiceModule.Port)

  def start(): Unit = {
    implicit val system = ActorSystem("ermine-service")

    val service = system.actorOf(Props(classOf[ErmineService], bindingModule))

    IO(Http) ! Http.Bind(service, "localhost", port = port)
  }
}

object HttpServer extends App {
  val config = ConfigFactory.load

  implicit val serviceConfig = new ServiceModule(config)

  val service = new HttpServer()

  service.start()
}
