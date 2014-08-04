import Dependencies._

name := "extraction-manager-service"

description := "Http service for Ermine"

mainClass := Some("org.allenai.extraction.service.HttpServer")

libraryDependencies ++= AkkaLibraries ++ TestLibraries ++ Seq(
  allenaiCommon,
  allenaiWebapp,
  sprayJson,
  sprayModule("can"),
  sprayModule("client"),
  sprayModule("routing"),
  subcut,
  typesafeConfig)

addLoggingDependencies(libraryDependencies)

// Make sure we get the javaOptions we've set when we run.
fork in run := true

// Don't create windows or linux startup scripts.
NativePackagerKeys.makeBatScript := None

NativePackagerKeys.makeBashScript := None

// Copy the tagger data files to EC2.
deployDirs ++= Seq("data")
