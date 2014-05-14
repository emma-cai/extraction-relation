name := "extraction-api"

description := "API for Ermine extraction"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(openNlpCore, sprayJson)

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4"
)
