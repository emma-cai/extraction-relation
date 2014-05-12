name := "extraction-manager-service"

description := "Http service for Ermine"

// SBT native packager configs.
packageArchetype.java_application

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Revolver.reStart := Some("org.allenai.extraction.service.HttpServer")

libraryDependencies ++= loggingImplementations ++ ferretDeps ++ Seq(akkaActor, akkaLogging,
  typesafeConfig, sprayCan, sprayRouting, sprayClient, sprayJson, allenaiCommon, subcut) ++ testLibs

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4",
  "org.slf4j" % "slf4j-api" % "1.7.6",
  // Override the scopt library used by nlptools.
  // Unsafe only if we try to run an nlptools class that uses scopt (unlikely).
  "com.github.scopt" % "scopt_2.10" % "3.2.0",
  // subcut & scalatest depends on the latest version of scala-reflect, while subcut (transitively)
  // depends on an older version (2.10.0). Override to the latest version.
  // TODO(jkinkead): Update to the newest subcut release (2.1) once it's published.
  "org.scala-lang" % "scala-reflect" % "2.10.3"
)

// Make sure we get the javaOptions we've set when we run.
fork in run := true

// Set java options for run & re-start.
javaOptions ++= ermineJavaOptions ++ prologLibraryFlags ++
  Seq("-Dlogback.configurationFile=src/main/resources/logback.xml",
    "-Dferret.directory=../ermine/src/main/prolog")

Deploy.settings

// Copy the prolog scripts from Ermine to the universal staging directory.
mappings in Universal ++=
  ((sourceDirectory in ermine).value / "main" / "prolog" ** "*" x
    rebase((sourceDirectory in ermine).value / "main" / "prolog", "prolog/"))

// Copy the prolog scripts to EC2.
Deploy.deployDirs += "prolog"
