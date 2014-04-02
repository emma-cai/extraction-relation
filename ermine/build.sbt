name := "extraction-manager"

description := "Extraction management system"

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Revolver.reStart := Some("org.allenai.extraction.manager.Ermine")

libraryDependencies ++= loggingImplementations ++ ferretDeps ++ Seq(akkaActor, typesafeConfig,
  sprayJson, allenaiCommon)

// Up memory and add prolog path.
javaOptions ++= prologLibraryFlags ++ Seq("-Xmx3G", "-Xms3G")
