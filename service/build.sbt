import Dependencies._
import NativePackagerHelper.directory

name := "extraction-manager-service"

description := "Http service for Ermine"

mainClass := Some("org.allenai.extraction.service.HttpServer")

Deploy.settings

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

// Set java options for run & re-start.
javaOptions ++= ErmineMemory ++ Seq(
  s"-Dermine.dataDirectory=${(sourceDirectory in ermine).value}/main/data",
  s"-Dlogback.configurationFile=${baseDirectory.value}/conf/local_logback.xml",
  s"-Dconfig.file=${baseDirectory.value}/conf/local_application.conf"
  )

// Don't create windows or linux startup scripts.
NativePackagerKeys.makeBatScript := None

NativePackagerKeys.makeBashScript := None

// Copy the tagger config files to the universal staging directory.
mappings in Universal ++= directory((sourceDirectory in ermine).value / "main" / "data")

// Copy the ferret pipeline to the conf directory.
mappings in Universal += {
  ((baseDirectory in ermine).value / "pipelines" / "ferret.conf") -> "conf/ferret.conf"
}

// Copy the tagger data files to EC2.
Deploy.deployDirs ++= Seq("data")
