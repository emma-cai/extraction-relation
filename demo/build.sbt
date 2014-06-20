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

javaOptions ++= Seq("-Xmx1G", "-XX:+UseConcMarkSweepGC")

fork in run := true

Deploy.settings

mappings in Universal ++= directory(baseDirectory.value / "public")
