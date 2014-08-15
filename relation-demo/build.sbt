import Dependencies._

libraryDependencies ++= AkkaLibraries ++ TestLibraries ++ Seq(
  allenaiCommon,
  allenaiWebapp,
  nlpstackCore,
  nlpstackLemmatize,
  nlpstackParse,
  sprayJson,
  scopt,
  subcut,
  taggers,
  tinkerpop,
  sprayModule("can"),
  sprayModule("client"),
  sprayModule("routing"),
  typesafeConfig,
  dom4j,
  poiFinal,
  poiOoxml,
  poiOoxmlSchema,
  xmlBeans, 
  weka, 
  solvers, 
  textualEntailment,  
  ari, 
  //ermineApi,
  //gremlin,
  //"com.googlecode.aima-java" % "aima-core" % "0.10.5",
  //morpha,
  //datastoreCommon,
  //datastoreClient,
  "org.apache.lucene" % "lucene-core" % "2.9.4")

addLoggingDependencies(libraryDependencies)

// Force npm:build when using sbt-revolver re-start to ensure UI is built
Revolver.reStart <<= (Revolver.reStart).dependsOn(NodeKeys.build in Npm)
