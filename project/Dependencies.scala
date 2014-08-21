import sbt._

/** Object holding the dependencies Common has, plus resolvers and overrides. */
object Dependencies {
  val Resolvers = Seq(
    "AllenAI Snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots",
    "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
    "Restlet repo" at "http://maven.restlet.org",
    "spray repo" at "http://repo.spray.io",
    // Factorie resolver.
    "IESL Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public",
    Resolver.sonatypeRepo("snapshots"))

  val slf4jVersion = "1.7.7"
  // The logging API to use. This should be the only logging dependency of any API artifact
  // (anything that's going to be depended on outside of this SBT project).
  val slf4jApi = "org.slf4j" % "slf4j-api" % slf4jVersion
  // Removes all unneeded log4j & slf4j implementations from the given modules. Adds in a single
  // slf4j implementation (logback), and the log4j -> slf4j bridge.
  // This should be called on libraryDependencies like:
  // addLoggingDependencies(libraryDependencies)
  def addLoggingDependencies(deps: SettingKey[Seq[ModuleID]]): Seq[Setting[Seq[ModuleID]]] = {
    val cleanedDeps = deps ~= { seq =>
      seq map { module =>
        // Exclude the transitive dependencies that might mess things up for us.
        // slf4j replaces log4j.
        (module exclude("log4j", "log4j")
           // We're using logback as the slf4j implementation, and we're providing it below.
           exclude("org.slf4j", "slf4j-log4j12")
           exclude("org.slf4j", "slf4j-jdk14")
           exclude("org.slf4j", "slf4j-jcl")
           exclude("org.slf4j", "slf4j-simple")
           // We'll explicitly provide the logback version; this avoids having to do an override.
           exclude("ch.qos.logback", "logback-core")
           exclude("ch.qos.logback", "logback-classic"))
      }
    }
    // Now, add the logging libraries.
    val logbackDeps = deps ++= Seq(
      slf4jApi,
      // Bridge log4j logging to slf4j.
      "org.slf4j" % "log4j-over-slf4j" % slf4jVersion,
      // Use logback for the implementation.
      "ch.qos.logback" % "logback-core" % "1.1.2",
      "ch.qos.logback" % "logback-classic" % "1.1.2")
    Seq(cleanedDeps, logbackDeps)
  }

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
    ("org.allenai.nlpstack" %% s"nlpstack-${id}" % "0.6" exclude("junit", "junit"))
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

  // Classification libraries
  //val weka = "nz.ac.waikato.cms.weka" % "weka-stable" % "3.6.6"
  //val solvers = "org.allenai.ari.solvers" %% "solvers-inference" % "0.0.2-SNAPSHOT"
  //val textualEntailment = ("org.allenai.textual-entailment" %% "interface" % "2013.03.05-0-SNAPSHOT")
	//.exclude("org.allenai.nlptools", "core_2.10")
  val dom4j = "dom4j" % "dom4j" % "1.6.1"
  val poiFinal = "org.apache.poi" % "poi" % "3.10-FINAL"
  val poiOoxml = "org.apache.poi" % "poi-ooxml" % "3.10-FINAL"
  val poiOoxmlSchema = "org.apache.poi" % "poi-ooxml-schemas" % "3.10-FINAL"
  val xmlBeans = "org.apache.xmlbeans" % "xmlbeans" % "2.3.0"
  val weka = "nz.ac.waikato.cms.weka" % "weka-stable" % "3.6.6"
  val solvers = "org.allenai.ari.solvers" %% "solvers-inference" % "0.0.2-SNAPSHOT"
  val textualEntailment = ("org.allenai.textual-entailment" %% "interface" % "2013.03.05-0-SNAPSHOT")
    .exclude("org.allenai.nlptools", "core_2.10")  
  val ari = "org.allenai.ari" %% "ari-interface" % "2014.06.10-0-SNAPSHOT"
  //val ermineApi = "org.allenai.extraction" %% "extraction-api" % "2014.5.14-0-SNAPSHOT"
  //val gremlin = "com.michaelpollmeier" %% "gremlin-scala" % "2.5.0"
  //val datastoreCommon = "org.allenai.ari-datastore" %% "interface" % "2014.5.16-0-SNAPSHOT"
  //val datastoreClient = "org.allenai.ari-datastore" %% "client" % "2014.5.16-0-SNAPSHOT"
  //val morpha = "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.4"

  // Demo-only libraries.

  // For http calls.
  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % "0.11.0"
  // For extracting text from URLs.
  val boilerpipe = "com.syncthemall" % "boilerpipe" % "1.2.2"

  // Dependencies for using H2 (database)
  val H2DatabaseLibraries = Seq(
    "com.h2database" % "h2" % "1.3.175",
    "com.typesafe.slick" %% "slick" % "2.0.0")

  // Akka actors & logging.
  def akkaModule(id: String) = "com.typesafe.akka" %% s"akka-${id}" % "2.3.2"
  val AkkaLibraries = Seq(akkaModule("actor"), akkaModule("slf4j"))

  // Clear libraries, for use in SRL.
  val clearNlp = "com.clearnlp" % "clearnlp" % "2.0.2"
  val ClearLibraries = Seq(clearNlp,
      "com.clearnlp" % "clearnlp-dictionary" % "1.0",
      "com.clearnlp" % "clearnlp-general-en-srl" % "1.1")

  // Stanford parser dependencies.
  val StanfordLibraries = Seq(
    // Kevin's patches of the Stanford parser.
    "org.allenai.corenlp" % "stanford-corenlp" % "3.2.0.1",
    // Dependency that the Stanford parser relies on. This also pulls in the
    // other dependencies the parser needs.
    "edu.stanford.nlp" % "stanford-corenlp" % "3.2.0" classifier("models")
  )

  // Collection of libraries used in test only.
  val TestLibraries = Seq(
    // Mockito for test mocks.
    "org.mockito" % "mockito-all" % "1.9.5" % "test",
    // Common testkit.
    "org.allenai.common" %% "common-testkit" % "2014.06.10-0-SNAPSHOT" % "test")

  val Overrides = Set(
    // Base version of the scala core libraries; minor override.
    "com.carrotsearch" % "hppc" % "0.4.1", 
    "args4j" % "args4j" % "2.0.16", 
    //"com.github.scopt" % "scopt_2.10" % "2.1.0", 
    "com.github.scopt" % "scopt_2.10" % "3.2.0", 
    "commons-codec" % "commons-codec" % "1.5",
    "org.scala-lang" % "scala-library" % "2.10.4",
    "org.scala-lang" % "scala-compiler" % "2.10.4",
    "org.scala-lang" % "scala-reflect" % "2.10.4",
    // Various things depend transitively on different versions of slf4j. Override to the version
    // Akka wants.
    "org.slf4j" % "slf4j-api" % "1.7.7",
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
