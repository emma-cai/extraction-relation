import Dependencies._

name := "extraction-api"

description := "API for Ermine extraction"

libraryDependencies ++= Seq(allenaiCommon, sprayJson) ++ H2DatabaseLibraries
