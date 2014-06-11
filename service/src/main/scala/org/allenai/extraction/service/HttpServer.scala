package org.allenai.extraction.service

import org.allenai.extraction.ActorSystemModule
import org.allenai.extraction.manager.ErmineModule

import akka.actor.ActorSystem
import akka.io.IO
import com.escalatesoft.subcut.inject.{ BindingModule, Injectable }
import spray.can.Http

class HttpServer(implicit val bindingModule: BindingModule) extends Injectable {
  val port = inject[Int](ServiceModuleId.Port)
  val actorSystem = inject[ActorSystem]

  def start(): Unit = {
    implicit val system = actorSystem
    val service = system.actorOf(ErmineService.props(bindingModule))

    IO(Http) ! Http.Bind(service, "0.0.0.0", port = port)
  }
}

object HttpServer extends App {
  val system = ActorSystem("ermine-service")

  val serviceConfig = (new ServiceModule(system) ~
    new ErmineModule(system) ~
    new ActorSystemModule(system))

  val service = new HttpServer()(serviceConfig)

  service.start()
}
