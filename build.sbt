import NativePackagerHelper.directory

lazy val root = Project(id = "extraction-root", base = file(".")).settings (
  publish := { },
  publishLocal := { })
  .aggregate(api, demo, ermine, service)

lazy val api = Project(
  id = "api",
  base = file("api"),
  settings = ExtractionBuild.buildSettings)
  .enablePlugins(FormatPlugin, VersionInjectorPlugin, TravisPublisherPlugin)

lazy val demo = Project(
  id = "demo",
  base = file("demo"),
  settings = ExtractionBuild.buildSettings)
  .dependsOn(api)
  .enablePlugins(FormatPlugin, VersionInjectorPlugin, TravisPublisherPlugin, DeployPlugin)

lazy val ermine = Project(
  id = "ermine",
  base = file("ermine"),
  settings = ExtractionBuild.buildSettings)
  .dependsOn(api)
  .enablePlugins(FormatPlugin, VersionInjectorPlugin, TravisPublisherPlugin)

lazy val service = Project(
  id = "service",
  base = file("service"),
  settings = ExtractionBuild.buildSettings ++ Seq(
    // Set java options for run & re-start.
    javaOptions ++= ExtractionBuild.ErmineMemory ++ Seq(
      s"-Dermine.dataDirectory=${(sourceDirectory in ermine).value}/main/data",
      s"-Dlogback.configurationFile=${baseDirectory.value}/conf/local_logback.xml",
      s"-Dconfig.file=${baseDirectory.value}/conf/local_application.conf"
    ),
    // Copy the tagger config files to the universal staging directory.
    mappings in Universal ++= directory((sourceDirectory in ermine).value / "main" / "data"),
    // Copy the ferret pipeline to the conf directory.
    mappings in Universal += {
      ((baseDirectory in ermine).value / "pipelines" / "ferret.conf") -> "conf/ferret.conf"
    }
  ))
  .dependsOn(api, ermine)
  .enablePlugins(FormatPlugin, VersionInjectorPlugin, TravisPublisherPlugin, DeployPlugin)

lazy val relationDemo = Project(
  id = "relation-demo",
  base = file("relation-demo"),
  settings = ExtractionBuild.buildSettings)
  .dependsOn(api)
  .enablePlugins(WebappPlugin)
