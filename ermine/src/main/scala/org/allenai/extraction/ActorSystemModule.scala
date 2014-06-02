package org.allenai.extraction

import akka.actor.ActorSystem
import akka.event.Logging
import akka.event.LoggingAdapter
import akka.event.LogSource
import com.escalatesoft.subcut.inject.NewBindingModule

/** Module providing the ActorSystem for an app.
  * @param system the actor system to provide
  */
// TODO(jkinkead): Move to a common library if we continue to use subcut.
class ActorSystemModule(system: ActorSystem) extends NewBindingModule(module => {
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
