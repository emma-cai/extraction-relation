import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object ExtractionBuild {
  /** Increased memory to handle Stanford parser. */
  val ErmineMemory = Seq("-Xmx3G", "-Xms3G")

  val inheritedSettings = Revolver.settings ++ Publish.settings

  val buildSettings = inheritedSettings ++ Seq(
    organization := "org.allenai.extraction",
    crossScalaVersions := Seq("2.10.4"),
    scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head },
    scalacOptions ++= Seq("-Xlint", "-feature", "-unchecked", "-deprecation"),
    conflictManager := ConflictManager.strict,
    dependencyOverrides ++= Dependencies.Overrides,
    resolvers ++= Dependencies.Resolvers,
    homepage := Some(url("http://github.com/allenai/extraction")))
}
