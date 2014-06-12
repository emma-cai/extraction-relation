import sbt._

import java.io.File
import java.io.IOException
import scala.util.Try

/** Helper to set up JPL paths. */
object Prolog {
  /** Returns the path the prolog installation, or None if a prolog installation can't be found. */
  def getPrologPath(): Option[String] = {
    try {
      // --dump-runtime-variables returns an eval-able string; so the values are quoted & have
      // semicolons at the end.
      val ShellAssignment = """([^=]+)="(.*)";""".r
      val envMap = (for {
        // TODO(jkinkead): The script that comes bundled with swi-prolog also checks `swi-prolog`
        // and `pl`, which we could do here.
        output <- Try(Process(Seq("swipl", "--dump-runtime-variables")).lines).toOption.toSeq
        line <- output
        (key, value) <- line match {
          case ShellAssignment(key, value) => Some(key -> value)
          case _ => None
        }
      } yield (key -> value)).toMap
      for {
        // Base install directory.
        plbase <- envMap.get("PLBASE")
        // Architecture label.
        plarch <- envMap.get("PLARCH")
      } yield s"${plbase}/lib/${plarch}"
    } catch {
        // Occurs if "swipl" isn't found.
        case ioe: IOException => None
    }
  }

  // Seq of the required flag, or empty if we failed to find prolog.
  val LibraryFlags = getPrologPath() match {
    case Some(path) => {
      println(s"Using swipl at ${path}")

      if (!(new File(s"${path}/libjpl.jnilib").exists)) {
        println("WARNING: Couldn't find libjpl - did you install swipl --with-jpl?")
      }
      Seq(s"-Djava.library.path=${path}")
    }
    case None => {
      println("WARNING: Couldn't find swipl - prolog will fail at runtime!")
      Seq.empty
    }
  }
}
