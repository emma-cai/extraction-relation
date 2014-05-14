addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

// Native packager, for doing deploys.
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.7.0-RC2")

// Revolver, for auto-reloading of changed files in sbt.
// See https://github.com/spray/sbt-revolver .
addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

resolvers += "allenai nexus repository" at "http://utility.allenai.org:8081/nexus/content/repositories/releases"

resolvers += "allenai nexus repository snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots"

credentials += Credentials("Sonatype Nexus Repository Manager", "utility.allenai.org", "deployment", "answermyquery")

lazy val ai2PluginsVersion = "2014.2.20-2-SNAPSHOT"

// Automates injection of artifact / git version info
addSbtPlugin("org.allenai.plugins" % "sbt-version-injector" % ai2PluginsVersion)

addSbtPlugin("org.allenai.plugins" % "sbt-travis-publisher" % ai2PluginsVersion)

addSbtPlugin("org.allenai.plugins" % "sbt-shared-ui" % ai2PluginsVersion)

addSbtPlugin("org.allenai.plugins" % "sbt-deploy" % "2014.4.14-1-SNAPSHOT")
