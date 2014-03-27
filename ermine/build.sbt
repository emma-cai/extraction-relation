name := "extraction-manager"

description := "Extraction management system"

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Revolver.reStart := Some("org.allenai.extraction.manager.Ermine")

libraryDependencies ++= Seq(akkaActor, typesafeConfig, sprayClient, stanfordModels, stanfordPatched)

javaOptions ++= Seq("-Xmx3G", "-Xms3G")
