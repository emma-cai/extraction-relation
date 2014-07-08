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
  val aristore = ("org.allenai.ari-datastore" %% "client" % "2014.5.16-0-SNAPSHOT"
    exclude("com.typesafe.akka", "akka-actor_2.10"))
  val taggers = ("org.allenai.taggers" %% "taggers-core" % "0.5-SNAPSHOT"
    exclude("com.clearnlp", "clearnlp")
    exclude("com.github.scopt", "scopt_2.10"))
  def nlpstackModule(id: String) = {
    ("org.allenai.nlpstack" %% s"nlpstack-${id}" % "2014.6.23-1-SNAPSHOT"
      exclude("junit", "junit"))
  }
  val nlpstackCore = nlpstackModule("core")
  val nlpstackParse = nlpstackModule("parse")
  val nlpstackLemmatize = nlpstackModule("lemmatize")

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

  // Dependencies for using H2 (database)
  val H2DatabaseLibraries = Seq(
    "com.h2database" % "h2" % "1.3.175",
    "com.typesafe.slick" %% "slick" % "2.0.0")

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
    // Base version of the scala core libraries; minor override.
    "org.scala-lang" % "scala-library" % "2.10.4",
    "org.scala-lang" % "scala-compiler" % "2.10.4",
    "org.scala-lang" % "scala-reflect" % "2.10.4",
    // Various things depend transitively on different versions of slf4j. Override to the version
    // Akka wants.
    "org.slf4j" % "slf4j-api" % "1.7.5",
    // Similarly, log4j is depended on by lots of stuff. Override to the newest requested version.
    "log4j" % "log4j" % "1.2.17",
    // Somehow we get two versions of commons-io through ari-datastore-client - use the most recent
    // one.
    "commons-io" % "commons-io" % "2.4",
    // Solr and jsonld both depend on a different version; prefer the more recent.
    "org.apache.httpcomponents" % "httpclient" % "4.3.1",
    // httpcomponents needs more recent commons-logging
    "commons-logging" % "commons-logging" % "1.1.3",
    // There's a phantom dependency on 14.0; override to the taggers dependency.
    "com.google.guava" % "guava" % "15.0",
    // There's an internal (jackson -> jackson) dependency that needs overriding.
    "com.fasterxml.jackson.core" % "jackson-core" % "2.2.3")
}
