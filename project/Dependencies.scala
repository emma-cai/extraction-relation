import sbt._

/** Object holding the dependencies Common has, plus resolvers and overrides. */
object Dependencies {
  val Resolvers = Seq(
    "AllenAI Snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots",
    "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
    "Restlet repo" at "http://maven.restlet.org",
    "spray repo" at "http://repo.spray.io",
    Resolver.sonatypeRepo("snapshots"))

  def sprayModule(id: String) = "io.spray" % s"spray-${id}" % "1.3.1"
  // spray-json uses a separate version number.
  val sprayJson = "io.spray" %% "spray-json" % "1.2.6"

  // AI2 common libs.
  val allenaiCommon = "org.allenai.common" %% "common-core" % "2014.06.10-0-SNAPSHOT"
  val allenaiWebapp = "org.allenai.common" %% "common-webapp" % "2014.06.10-0-SNAPSHOT"
  // Other AI2 libs used by Ermine.
  val aristore = "org.allenai.ari-datastore" %% "client" % "2014.5.16-0-SNAPSHOT"
  val taggers = "org.allenai.taggers" %% "taggers-core" % "0.5-SNAPSHOT"

  // Config, used everywhere.
  val typesafeConfig = "com.typesafe" % "config" % "1.2.0"
  // Scopt, for parsing commandline options. Used by Ermine commandline.
  val scopt = "com.github.scopt" % "scopt_2.10" % "3.2.0"
  // Subcut for dependency injection.
  val subcut = "com.escalatesoft.subcut" %% "subcut" % "2.0"
  // Tinkerpop for RDF graph manipulation.
  val tinkerpop = "com.tinkerpop.blueprints" % "blueprints-sail-graph" % "2.5.0"

  // Demo-only libraries.

  // For http calls.
  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % "0.11.0"
  // For extracting text from URLs.
  val boilerpipe = "com.syncthemall" % "boilerpipe" % "1.2.2"

  // Akka actors, logging, and backend for logging.
  def akkaModule(id: String) = "com.typesafe.akka" %% s"akka-${id}" % "2.3.2"
  val AkkaLibraries = Seq(akkaModule("actor"),
    akkaModule("slf4j"),
    "ch.qos.logback" % "logback-classic" % "1.0.13")

  // Clear libraries, for use in SRL.
  val clearNlp = "com.clearnlp" % "clearnlp" % "2.0.2"
  val ClearLibraries = Seq(clearNlp,
      "com.clearnlp" % "clearnlp-dictionary" % "1.0",
      "com.clearnlp" % "clearnlp-general-en-srl" % "1.1")

  // Collection of libraries used in test only.
  val TestLibraries = Seq(
    // Mockito for test mocks.
    "org.mockito" % "mockito-all" % "1.9.5" % "test",
    // Common testkit.
    "org.allenai.common" %% "common-testkit" % "2014.06.10-0-SNAPSHOT" % "test")

  val Overrides = Set(
    // Base version of the scala language; minor override.
    "org.scala-lang" % "scala-library" % "2.10.4",
    // subcut & scalatest depends on the latest version of scala-reflect, while subcut
    // (transitively) depends on an older version (2.10.0). Override to the latest version.
    // TODO(jkinkead): Update to the newest subcut release (2.1) once it's published.
    "org.scala-lang" % "scala-reflect" % "2.10.3",
    // Various things depend transitively on different versions of slf4j. Override to the version
    // Akka wants.
    "org.slf4j" % "slf4j-api" % "1.7.5",
    // Similarly, log4j is depended on by lots of stuff. Override to the newest requested version.
    "log4j" % "log4j" % "1.2.17",
    // Override the scopt library used by nlptools.
    // Unsafe only if we try to run an nlptools class that uses scopt (unlikely).
    "com.github.scopt" % "scopt_2.10" % "3.2.0",
    // Somehow we get two versions of commons-io through ari-datastore-client - use the most recent
    // one.
    "commons-io" % "commons-io" % "2.4",
    // Solr and jsonld both depend on a different version; prefer the more recent.
    "org.apache.httpcomponents" % "httpclient" % "4.3.1",
    // httpcomponents needs more recent commons-logging
    "commons-logging" % "commons-logging" % "1.1.3",
    // There's a phantom dependency on 14.0; override to the taggers dependency.
    "com.google.guava" % "guava" % "15.0",
    // Force most recent clear version.
    clearNlp,
    // Minor version override for akka actor.
    akkaModule("actor"))
}
