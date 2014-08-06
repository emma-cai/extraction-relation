import Dependencies._

libraryDependencies ++= AkkaLibraries ++ TestLibraries ++ Seq(
  allenaiCommon,
  allenaiWebapp,
  nlpstackCore,
  nlpstackLemmatize,
  nlpstackParse,
  sprayJson,
  scopt,
  subcut,
  taggers,
  tinkerpop,
  sprayModule("can"),
  sprayModule("client"),
  sprayModule("routing"),
  typesafeConfig, 
  "org.apache.lucene" % "lucene-core" % "2.9.4")

addLoggingDependencies(libraryDependencies)

// Force npm:build when using sbt-revolver re-start to ensure UI is built
Revolver.reStart <<= (Revolver.reStart).dependsOn(NodeKeys.build in Npm)
