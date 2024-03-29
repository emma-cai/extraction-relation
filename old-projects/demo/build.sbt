import Dependencies._
import NativePackagerHelper.directory

name := "extraction-demo"

description := "An interactive demo for extractors."

libraryDependencies ++= AkkaLibraries ++ Seq(
    allenaiCommon,
    boilerpipe,
    dispatch,
    typesafeConfig,
    // for web serving
    sprayModule("can"),
    sprayModule("routing"),
    sprayJson)

addLoggingDependencies(libraryDependencies)

javaOptions ++= Seq("-Xmx1G", "-XX:+UseConcMarkSweepGC")

fork in run := true

mappings in Universal ++= directory(baseDirectory.value / "public")

