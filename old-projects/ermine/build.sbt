import Dependencies._
import NativePackagerHelper.directory

name := "extraction-manager"

description := "Extraction management system"

mainClass := Some("org.allenai.extraction.manager.Ermine")

libraryDependencies ++= AkkaLibraries ++ ClearLibraries ++ StanfordLibraries ++ TestLibraries ++
  Seq(
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
    typesafeConfig,
    "org.apache.lucene" % "lucene-core" % "2.9.4",
    "nz.ac.waikato.cms.weka" % "weka-stable" % "3.6.6"
    )

addLoggingDependencies(libraryDependencies)

fork in run := true

javaOptions ++= Seq(
  // Set the config file.
  s"-Dconfig.file=${baseDirectory.value}/conf/application.conf",
  // Set the `baseDirectory` value, used in the config file.
  s"-DbaseDirectory=${baseDirectory.value}"
)