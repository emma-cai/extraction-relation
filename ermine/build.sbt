import Dependencies._
import NativePackagerHelper.directory

name := "extraction-manager"

description := "Extraction management system"

mainClass in Compile := Some("org.allenai.extraction.manager.Ermine")

Deploy.settings

// SBT native packager configs.
packageArchetype.java_application

libraryDependencies ++= AkkaLibraries ++ ClearLibraries ++ TestLibraries ++ ferretDeps ++ Seq(
  allenaiCommon,
  aristore,
  scopt,
  sprayJson,
  subcut,
  taggers,
  tinkerpop,
  typesafeConfig)

// Don't create windows startup script.
NativePackagerKeys.makeBatScript := None

// Copy the prolog scripts & tagger config files to the universal staging directory.
mappings in Universal ++= directory(sourceDirectory.value / "main" / "prolog")

mappings in Universal ++= directory(sourceDirectory.value / "main" / "data")

// Set java options in the native packager script. These are literals embedded in the script, so we
// have to call 'addJava' to get them added (and we quote them as well).
NativePackagerKeys.bashScriptExtraDefines ++=
  (ErmineMemory map { "addJava \"" +  _ + "\"" }) ++
  Seq("""
eval `swipl --dump-runtime-variables`
addJava "-Djava.library.path=${PLBASE}/lib/${PLARCH}"
addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"
addJava "-Dconfig.file=${app_home}/../conf/application.conf"
addJava "-Dapp_home=${app_home}"
""")
