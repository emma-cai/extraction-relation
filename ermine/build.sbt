import Dependencies._
import NativePackagerHelper.directory

name := "extraction-manager"

description := "Extraction management system"

mainClass in Compile := Some("org.allenai.extraction.manager.Ermine")

// SBT native packager configs.
// TODO(jkinkead): Get rid of the native packager install once we get rid of Prolog.
packageArchetype.java_application

libraryDependencies ++= AkkaLibraries ++ ClearLibraries ++ TestLibraries ++ ferretDeps ++ Seq(
  allenaiCommon,
  aristore,
  nlpstackCore,
  nlpstackLemmatize,
  nlpstackParse,
  scopt,
  sprayJson,
  subcut,
  taggers,
  tinkerpop,
  typesafeConfig)

addLoggingDependencies(libraryDependencies)

fork in run := true

// Set the config file.
javaOptions += s"-Dconfig.file=${baseDirectory.value}/conf/application.conf"

// Set the `baseDirectory` value, used in the config file.
javaOptions += s"-DbaseDirectory=${baseDirectory.value}"

// Don't create windows startup script.
NativePackagerKeys.makeBatScript := None

// Set java options in the native packager script. These are literals embedded in the script, so we
// have to call 'addJava' to get them added (and we quote them as well).
NativePackagerKeys.bashScriptExtraDefines ++=
  (ErmineMemory map { "addJava \"" +  _ + "\"" }) ++
  Seq("""
eval `swipl --dump-runtime-variables`
addJava "-Djava.library.path=${PLBASE}/lib/${PLARCH}"
addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"
addJava "-Dconfig.file=${app_home}/../../../../conf/application.conf"
addJava "-DbaseDirectory=${app_home}/../../../.."
""")
