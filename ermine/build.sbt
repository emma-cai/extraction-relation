name := "extraction-manager"

description := "Extraction management system"

// SBT native packager configs.
packageArchetype.java_application

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Revolver.reStart := Some("org.allenai.extraction.manager.Ermine")

libraryDependencies ++= loggingImplementations ++ ferretDeps ++ Seq(akkaActor, typesafeConfig,
  aristore, sprayJson, allenaiCommon, aristore, scopt, subcut, taggers) ++ testLibs

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-library" % "2.10.4",
  "log4j" % "log4j" % "1.2.17",
  "org.slf4j" % "slf4j-api" % "1.7.6",
  // subcut & scalatest depends on the latest version of scala-reflect, while subcut (transitively)
  // depends on an older version (2.10.0). Override to the latest version.
  // TODO(jkinkead): Update to the newest subcut release (2.1) once it's published.
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  // Override the scopt library used by nlptools.
  // Unsafe only if we try to run an nlptools class that uses scopt (unlikely).
  "com.github.scopt" % "scopt_2.10" % "3.2.0",
  // taggers (which is a dependency here) takes a dependency on. taggers looks for guava version
  // 14.0.1.
  "com.google.guava" % "guava" % "15.0",
  // tinkerpop needs later versions than solr
  "commons-io" % "commons-io" % "2.4",
  "org.apache.httpcomponents" % "httpclient" % "4.3.1",
  // Minor version override.
  akkaModule("actor")
)

// Don't create windows startup script.
NativePackagerKeys.makeBatScript := None

// Copy the prolog scripts to the universal staging directory.
mappings in Universal ++=
   (sourceDirectory.value / "main" / "prolog" ** "*" x
    rebase(sourceDirectory.value / "main" / "prolog", "prolog/"))

mappings in Universal ++=
   (sourceDirectory.value / "main" / "data" ** "*" x
    rebase(sourceDirectory.value / "main" / "data", "data/"))

// Set java options in the native packager script. These are literals embedded in the script, so we
// have to call 'addJava' to get them added (and we quote them as well).
NativePackagerKeys.bashScriptExtraDefines ++=
  (ermineJavaOptions map { "addJava \"" +  _ + "\"" }) ++
  Seq("""
eval `swipl --dump-runtime-variables`
addJava "-Djava.library.path=${PLBASE}/lib/${PLARCH}"
addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"
addJava "-Dconfig.file=${app_home}/../conf/application.conf"
addJava "-Dapp_home=${app_home}"
""")
