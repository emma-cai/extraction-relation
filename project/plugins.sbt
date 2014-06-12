// Adds 'release' command, for publishing non-SNAPSHOT artifacts.
addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.3")

// Native packager, for doing deploys.
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.7.0-RC2")

// Revolver, for auto-reloading of changed files in sbt.
// See https://github.com/spray/sbt-revolver .
addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

// Configure resolvers for our AI2 plugins.
resolvers += "allenai nexus repository" at "http://utility.allenai.org:8081/nexus/content/repositories/releases"

resolvers += "allenai nexus repository snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots"

credentials += Credentials("Sonatype Nexus Repository Manager", "utility.allenai.org", "deployment", "answermyquery")

// Provides 'deploy' command to push to EC2.
addSbtPlugin("org.allenai.plugins" % "sbt-deploy" % "2014.4.14-1")

// Provides 'format' command & auto-checks formatting of files on compile.
addSbtPlugin("org.allenai.plugins" % "sbt-format" % "2014.5.9-1")

// Publishes artifacts when Travis builds complete on master.
addSbtPlugin("org.allenai.plugins" % "sbt-travis-publisher" % "2014.2.24-1")

// Automates injection of artifact / git version info.
addSbtPlugin("org.allenai.plugins" % "sbt-version-injector" % "2014.5.08-1")
