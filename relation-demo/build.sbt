import Dependencies._

libraryDependencies ++= AkkaLibraries ++ TestLibraries ++ Seq(
  allenaiCommon,
  allenaiWebapp,
  sprayJson,
  sprayModule("can"),
  sprayModule("client"),
  sprayModule("routing"),
  typesafeConfig)

addLoggingDependencies(libraryDependencies)

// Force npm:build when using sbt-revolver re-start to ensure UI is built
Revolver.reStart <<= (Revolver.reStart).dependsOn(NodeKeys.build in Npm)
