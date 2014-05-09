package org.allenai.extraction.service

import org.allenai.extraction.manager.ErmineModule

import akka.actor.{ ActorSystem, Props }
import akka.io.IO
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import spray.can.Http

class HttpServer(implicit val bindingModule: BindingModule) extends Injectable {
  val port = inject [Int](ServiceModuleId.Port)

  def start(): Unit = {
    implicit val system = ActorSystem("ermine-service")

    val service = system.actorOf(Props(classOf[ErmineService], bindingModule))

    IO(Http) ! Http.Bind(service, "0.0.0.0", port = port)
  }
}

object HttpServer extends App {
  implicit val serviceConfig = ServiceModule ~ ErmineModule

  val service = new HttpServer()

  service.start()
}
