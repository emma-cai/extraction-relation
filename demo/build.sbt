name := "extraction-demo"

description := "An interactive demo for extractors."

libraryDependencies ++= Seq(
    allenaiCommon,
    scopt,
    typesafeConfig,
    // for web serving
    akkaLogging,
    akkaActor,
    "io.spray" % "spray-can" % sprayVersion,
    "io.spray" % "spray-routing" % sprayVersion,
    // for http calls
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
    // for extracting text from URLs
    "com.syncthemall" % "boilerpipe" % "1.2.2",
    // for parsing/writing json
    sprayJson) ++ loggingImplementations

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4",
  "org.slf4j" % "slf4j-api" % "1.7.6"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

javaOptions += "-Xmx1G"

javaOptions += "-XX:+UseConcMarkSweepGC"

fork in run := true

Deploy.settings

mappings in Universal ++=
  (baseDirectory.value / "public" ** "*" pair relativeTo(baseDirectory.value))
