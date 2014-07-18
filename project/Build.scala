import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object ExtractionBuild extends Build {
  /** Increased memory to handle Stanford parser. */
  val ErmineMemory = Seq("-Xmx3G", "-Xms3G")

  val inheritedSettings = Defaults.defaultSettings ++ Format.settings ++ Revolver.settings ++
    Publish.settings ++ TravisPublisher.settings ++ VersionInjector.settings

  val buildSettings = inheritedSettings ++ Seq(
    organization := "org.allenai.extraction",
    crossScalaVersions := Seq("2.10.4"),
    scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head },
    scalacOptions ++= Seq("-Xlint", "-feature", "-unchecked", "-deprecation"),
    conflictManager := ConflictManager.strict,
    dependencyOverrides ++= Dependencies.Overrides,
    resolvers ++= Dependencies.Resolvers,
    homepage := Some(url("http://github.com/allenai/extraction")))

  lazy val root = Project(id = "extraction-root", base = file(".")).settings (
    publish := { },
    publishLocal := { }
  ).aggregate(api, demo, ermine, service)

  lazy val api = Project(
    id = "api",
    base = file("api"),
    settings = buildSettings)

  lazy val demo = Project(
    id = "demo",
    base = file("demo"),
    settings = buildSettings
  ).dependsOn(api)

  lazy val ermine = Project(
    id = "ermine",
    base = file("ermine"),
    settings = buildSettings
  ).dependsOn(api)

  lazy val service = Project(
    id = "service",
    base = file("service"),
    settings = buildSettings
  ).dependsOn(api, ermine)
}
