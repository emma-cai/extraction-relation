import Dependencies._

name := "extraction-manager-service"

description := "Http service for Ermine"

mainClass in Revolver.reStart := Some("org.allenai.extraction.service.HttpServer")

Deploy.settings

// SBT native packager configs.
packageArchetype.java_application

libraryDependencies ++= AkkaLibraries ++ TestLibraries ++ Seq(
  allenaiCommon,
  allenaiWebapp,
  sprayJson,
  sprayModule("can"),
  sprayModule("client"),
  sprayModule("routing"),
  subcut,
  typesafeConfig)

// Make sure we get the javaOptions we've set when we run.
fork in run := true

// Set java options for run & re-start.
javaOptions ++= ErmineMemory ++ Prolog.LibraryFlags ++
  Seq("-Dlogback.configurationFile=src/main/resources/logback.xml",
    "-Dferret.directory=../ermine/src/main/prolog")

// Don't create windows or linux startup scripts.
NativePackagerKeys.makeBatScript := None

NativePackagerKeys.makeBashScript := None

// Copy the prolog scripts from Ermine to the universal staging directory.
mappings in Universal ++=
  ((sourceDirectory in ermine).value / "main" / "prolog" ** "*" x
    rebase((sourceDirectory in ermine).value / "main" / "prolog", "prolog/"))

// Copy the taggers config files from Ermine to the universal staging directory.
mappings in Universal ++=
  ((sourceDirectory in ermine).value / "main" / "data" ** "*" x
    rebase((sourceDirectory in ermine).value / "main" / "data", "data/"))

// Copy the prolog scripts & tagger data files to EC2.
Deploy.deployDirs ++= Seq("prolog", "data")
