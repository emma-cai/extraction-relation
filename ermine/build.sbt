name := "extraction-manager"

description := "Extraction management system"

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Revolver.reStart := Some("org.allenai.extraction.manager.Ermine")

libraryDependencies ++= Seq(akkaActor, jpl, typesafeConfig, sprayClient,
  stanfordModels, stanfordPatched)

// TODO(jkinkead): Fix the path settings to use something like the below:
// eval `$PL -dump-runtime-variables`
// java.library.path="$PLBASE/lib/$PLARCH"
javaOptions ++= Seq("-Xmx3G", "-Xms3G", "-Djava.library.path=/usr/local/Cellar/swi-prolog/6.6.4/libexec/lib/swipl-6.6.4/lib/x86_64-darwin13.1.0")
