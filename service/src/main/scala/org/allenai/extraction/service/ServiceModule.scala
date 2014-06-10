package org.allenai.extraction.service

import org.allenai.common.Config._
import org.allenai.common.Version
import org.allenai.common.webapp.InfoRoute
import org.allenai.extraction.ConfigModule

import akka.actor.ActorSystem
import akka.event.Logging
import com.escalatesoft.subcut.inject.{ BindingId, NewBindingModule }
import com.typesafe.config.Config

/** Binding IDs for ServiceModule. */
object ServiceModuleId {
  /** IDs the pipeline Config object. */
  object Pipelines extends BindingId
  /** IDs the Int for the server port. */
  object Port extends BindingId
}

/** Module injecting the configuration for the Ermine service. */
class ServiceModule(actorSystem: ActorSystem) extends NewBindingModule(module => {
  import module._

  val log = Logging(actorSystem, classOf[ServiceModule])

  // Load our binding for Config.
  val config = ConfigModule.inject[Config](None)

  // Port the service will run on.
  bind[Int] idBy ServiceModuleId.Port toSingle config[Int]("ermine.port")

  // Pipeline configurations.
  bind[Config] idBy ServiceModuleId.Pipelines toSingle config[Config]("ermine.pipelines")

  // Load our version & create the info route for it.
  try {
    val version = Version.fromResources("org.allenai.extraction", "extraction-manager-service")
    val infoRoute = new InfoRoute().withVersion(version)
    bind[InfoRoute] toSingle infoRoute
  } catch {
    case e: IllegalArgumentException => log.error(e, "failure loading infoRoute:")
  }
})
