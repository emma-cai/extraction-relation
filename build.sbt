import NativePackagerHelper.directory

lazy val relationDemo = Project(
  id = "relation-demo",
  base = file("relation-demo"),
  settings = ExtractionBuild.buildSettings)
//  .dependsOn(api)
  .enablePlugins(WebappPlugin)
