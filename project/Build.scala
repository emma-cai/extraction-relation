import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object ExtractionBuild extends Build {
  /** Increased memory to handle Stanford parser. */
  val ErmineMemory = Seq("-Xmx3G", "-Xms3G")

  // Full requirements for Stanford + Prolog extractions.
  val ferretDeps = {
    // Prolog interface jar. This also requires having prolog installed to work -
    // see http://www.swi-prolog.org/build/macos.html
    val jpl = "org.allenai.jpl" % "jpl" % "6.6.4"

    // Kevin's patches of the Stanford parser.
    val stanfordPatched = "org.allenai.corenlp" % "stanford-corenlp" % "3.2.0.1"
    // Dependency that the Stanford parser relies on. This also pulls in the
    // other dependencies the parser needs.
    val stanfordModels = "edu.stanford.nlp" % "stanford-corenlp" % "3.2.0" classifier("models")

    val tinkerpop = "com.tinkerpop.blueprints" % "blueprints-sail-graph" % "2.5.0"

    Seq(jpl, stanfordPatched, stanfordModels, tinkerpop)
  }

  val inheritedSettings = Defaults.defaultSettings ++ Format.settings ++ Revolver.settings ++
    Publish.settings ++ TravisPublisher.settings ++ Deploy.settings ++ VersionInjector.settings

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

  // We need to include the main class setting at this level in order for the universal packager to
  // generate a start script for us.
  val ermineMainClass = mainClass in Compile := Some("org.allenai.extraction.manager.Ermine")
  lazy val ermine = Project(
    id = "ermine",
    base = file("ermine"),
    settings = buildSettings :+ ermineMainClass
  ).dependsOn(api)

  lazy val service = Project(
    id = "service",
    base = file("service"),
    settings = buildSettings
  ).dependsOn(api, ermine)
}
