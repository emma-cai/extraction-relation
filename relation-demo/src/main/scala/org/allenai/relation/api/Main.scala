package org.allenai.relation.api

object Main extends App {

  Classifier.run("fourth graders are planning a roller-skate race. blacktop would be the best for this race.", "blacktop", "this race")
  //  Classifier.run("a stove is used to boil water in a pan is an example in which a thermometer could be used to show that heat energy is being transferred.", "stove is used to boil water in a pan", "thermometer could be used")
  //  Classifier.run("a claws for obtaining food is correctly paired with its function.", "claws", "obtaining food")

  System.exit(0)
}