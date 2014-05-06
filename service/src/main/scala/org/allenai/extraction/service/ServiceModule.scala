package org.allenai.extraction.service

import org.allenai.common.Config._

import com.escalatesoft.subcut.inject.{ BindingId, NewBindingModule }
import com.typesafe.config.Config

/** Module injecting the configuration for the Ermine service. */
class ServiceModule(config: Config) extends NewBindingModule (module => {
  import module._

  // TODO(jkinkead): Document exceptions thrown; catch.
  
  bind [Int] idBy ServiceModule.Port toSingle config[Int]("ermine.port")

  // Pipeline configurations.
  bind [Config] idBy ServiceModule.Pipelines toSingle config.getConfig("ermine.pipelines")

  // Bind the typesafe config for use in other parts of the app.
  bind [Config] toSingle config
})

object ServiceModule {
  /** IDs the pipeline Config object. */
  object Pipelines extends BindingId
  /** IDs the Int for the server port. */
  object Port extends BindingId
}
