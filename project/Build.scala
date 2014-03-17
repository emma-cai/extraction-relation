import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object ExtractionBuild extends Build {
  val sprayVersion = "1.2.0"

  val akkaVersion = "2.2.3"
  def akkaModule(id: String) = "com.typesafe.akka" %% s"akka-$id" % akkaVersion
  val akkaActor = akkaModule("actor")
  val akkaLogging = akkaModule("slf4j")

  val logbackVersion = "1.0.13"
  val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
  val loggingImplementations = Seq(logbackCore, logbackClassic)

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
        "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases"),
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
