import Dependencies._

name := "extraction-demo"

description := "An interactive demo for extractors."

Deploy.settings

libraryDependencies ++= AkkaLibraries ++ Seq(
    allenaiCommon,
    boilerpipe,
    dispatch,
    typesafeConfig,
    // for web serving
    sprayModule("can"),
    sprayModule("routing"),
    sprayJson)

javaOptions ++= Seq("-Xmx1G", "-XX:+UseConcMarkSweepGC")

fork in run := true

mappings in Universal ++=
  (baseDirectory.value / "public" ** "*" pair relativeTo(baseDirectory.value))
