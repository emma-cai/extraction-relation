import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object ExtractionBuild extends Build {
  val sprayVersion = "1.3.1"

  val akkaVersion = "2.3.0"
  def akkaModule(id: String) = "com.typesafe.akka" %% s"akka-$id" % akkaVersion
  val akkaActor = akkaModule("actor")
  val akkaLogging = akkaModule("slf4j")

  val logbackVersion = "1.1.1"
  val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
  val loggingImplementations = Seq(logbackCore, logbackClassic)

  val openNlpCore = "org.allenai.nlptools" %% "nlptools-core" % "2.5.0-SNAPSHOT"
  val sprayJson = "io.spray" %%  "spray-json" % "1.2.5"

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
}
