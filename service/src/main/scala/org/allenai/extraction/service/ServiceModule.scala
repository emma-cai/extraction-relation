package org.allenai.extraction.service

import org.allenai.common.Config._
import org.allenai.extraction.ConfigModule

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
object ServiceModule extends NewBindingModule(module => {
  import module._

  // Load our binding for Config.
  module <~ ConfigModule
  val config = inject[Config](None)
  
  // Port the service will run on.
  bind [Int] idBy ServiceModuleId.Port toSingle config[Int]("ermine.port")

  // Pipeline configurations.
  bind [Config] idBy ServiceModuleId.Pipelines toSingle config[Config]("ermine.pipelines")
})
