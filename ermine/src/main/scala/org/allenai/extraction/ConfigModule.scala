package org.allenai.extraction

import com.escalatesoft.subcut.inject.NewBindingModule
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

/** Module providing an instance of Config. This uses the default ConfigFactory.load method to
  * create it, so any clients should be sure to set the system property config.file.
  */
object ConfigModule extends NewBindingModule(module => {
  module.bind [Config] toSingle ConfigFactory.load
})
