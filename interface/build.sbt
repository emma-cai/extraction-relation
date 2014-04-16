name := "extraction-interface"

description := "Data models and API for extraction"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(openNlpCore, sprayJson)

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4"
)
