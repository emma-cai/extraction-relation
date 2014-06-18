import Dependencies._

name := "extraction-manager"

description := "Extraction management system"

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
  (ErmineMemory map { "addJava \"" +  _ + "\"" }) ++
  Seq("""
eval `swipl --dump-runtime-variables`
addJava "-Djava.library.path=${PLBASE}/lib/${PLARCH}"
addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"
addJava "-Dconfig.file=${app_home}/../conf/application.conf"
addJava "-Dapp_home=${app_home}"
""")
