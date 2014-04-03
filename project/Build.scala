import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

import java.io.File
import java.io.IOException
import scala.util.Try

object ExtractionBuild extends Build {
  /** Returns the path the prolog installation, or None if a prolog installation can't be found. */
  def getPrologPath(): Option[String] = {
    try {
      // --dump-runtime-variables returns an eval-able string; so the values are quoted & have
      // semicolons at the end.
      val ShellAssignment = """([^=]+)="(.*)";""".r
      val envMap = (for {
        // TODO(jkinkead): The script that comes bundled with swi-prolog also checks `swi-prolog`
        // and `pl`, which we could do here.
        success <- Try(Process(Seq("swipl", "--dump-runtime-variables")).lines).toOption.toSeq
        line <- success
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
  val prologLibraryFlags = getPrologPath() match {
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

  val sprayVersion = "1.3.1"

  val akkaVersion = "2.3.0"
  def akkaModule(id: String) = "com.typesafe.akka" %% s"akka-$id" % akkaVersion
  val akkaActor = akkaModule("actor")
  val akkaLogging = akkaModule("slf4j")

  val loggingImplementations = {
    val logbackVersion = "1.1.1"
    val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
    val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
    Seq(logbackCore, logbackClassic)
  }

  val openNlpCore = "org.allenai.nlptools" %% "nlptools-core" % "2.5.0-SNAPSHOT"
  val sprayClient = "io.spray" %  "spray-client" % sprayVersion
  val sprayJson = "io.spray" %%  "spray-json" % "1.2.5"
  val typesafeConfig = "com.typesafe" % "config" % "1.0.2"

  // Full requirements for Stanford + Prolog extractions.
  val ferretDeps = {
    // Prolog interface jar. This also requires having prolog installed to work -
    // see http://www.swi-prolog.org/build/macos.html
    val jpl = "jpl" % "jpl" % "3.1.4-alpha"

    // Kevin's patches of the Stanford parser.
    val stanfordPatched = "org.allenai.corenlp" % "stanford-corenlp" % "3.2.0.1"
    // Dependency that the Stanford parser relies on. This also pulls in the
    // other dependencies the parser needs.
    val stanfordModels = "edu.stanford.nlp" % "stanford-corenlp" % "3.2.0" classifier("models")

    Seq(jpl, stanfordPatched, stanfordModels)
  }

  val allenaiCommon = "org.allenai.common" %% "common" % "0.0.1-SNAPSHOT"

  lazy val root = Project(id = "extraction-root", base = file(".")).settings (
    publish := { },
    publishTo := Some("bogus" at "http://nowhere.com"),
    publishLocal := { }
  ).aggregate(demo)

  val buildSettings = Defaults.defaultSettings ++ Format.settings ++ Revolver.settings ++
    Seq(
      organization := "org.allenai.extraction.demo",
      crossScalaVersions := Seq("2.10.3"),
      scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head },
      scalacOptions ++= Seq("-unchecked", "-deprecation"),
      resolvers ++= Seq(
        "AllenAI Snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots",
        "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
        "spray" at "http://repo.spray.io",
        "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"),
      homepage := Some(url("http://github.com/allenai/extraction")))

  lazy val interface = Project(
    id = "interface",
    base = file("interface"),
    settings = buildSettings)

  lazy val demo = Project(
    id = "demo",
    base = file("demo"),
    settings = buildSettings)

  lazy val ermine = Project(
    id = "ermine",
    base = file("ermine"),
    settings = buildSettings
  ).dependsOn(interface)
}
