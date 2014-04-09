name := "extraction-manager"

description := "Extraction management system"

// SBT native packager configs.
packageArchetype.java_application

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Revolver.reStart := Some("org.allenai.extraction.manager.Ermine")

libraryDependencies ++= loggingImplementations ++ ferretDeps ++ Seq(akkaActor, typesafeConfig,
  sprayJson, allenaiCommon) ++ testLibs

// Make sure we get the javaOptions we've set when we run.
fork in run := true

// Set java options for run & re-start.
javaOptions ++= ermineJavaOptions ++ Seq("-Dconfig.file=src/universal/conf/application.conf",
  "-Dlogback.configurationFile=src/main/resources/logback.xml")

// Set java options in the native packager script. These are literals embedded in the script, so we
// have to call 'addJava' manually.
NativePackagerKeys.bashScriptExtraDefines ++=
  (ermineJavaOptions map { "addJava \"" +  _ + "\"" }) ++
  Seq("""addJava "-Dconfig.file=${app_home}/../conf/application.conf"""",
    """addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"""")
