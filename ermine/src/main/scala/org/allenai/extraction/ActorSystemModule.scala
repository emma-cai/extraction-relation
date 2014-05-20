package org.allenai.extraction

import akka.actor.ActorSystem
import akka.event.Logging
import akka.event.LoggingAdapter
import akka.event.LogSource
import com.escalatesoft.subcut.inject.NewBindingModule
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

/** Module providing the ActorSystem for an app.
  * @param name the name of the actor system to provide
  */
// TODO(jkinkead): Move to a common library if we continue to use subcut.
class ActorSystemModule(name: String) extends NewBindingModule(module => {
  val system = ActorSystem(name)
  module.bind[ActorSystem] toSingle system
  module.bind[LogProvider] toSingle new LogProvider(system)
})

/** Provider class for LoggingAdapter instances. This is injected for classes to fetch their own
  * loggers out of, if they can't extend ActorLogging (i.e. they aren't Actors).
  *
  * Usage:
  * val log = inject[LogProvider].getLog(this)
  */
class LogProvider(system: ActorSystem) {
  def getLog(source: AnyRef): LoggingAdapter = Logging(system, source.getClass)
}
