import Dependencies._

name := "extraction-api"

description := "API for Ermine extraction"

libraryDependencies ++= Seq(sprayJson) ++ H2DatabaseLibraries
