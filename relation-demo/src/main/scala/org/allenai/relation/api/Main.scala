package org.allenai.relation.api

object Main {
  def main(args: Array[String]) = {
    //	  println(Classifier.testInstance("all animals do need water, air, and food in order to survive.", "requirement", "food", "survive", "1"))
    //	  println(Classifier.testInstance("all animals do need water, air, and food in order to survive.", "cause", "water", "survive", "1"))
    //	  println(Classifier.testInstance("the functions of a plants roots are to support the plant and take in water and nutrients.", "purpose", "plants roots", "support", "1"))
    Classifier.run("all animals do need water, air, and food in order to survive.", "water, air and food", "survive")
    Classifier.run("a stove is used to boil water in a pan is an example in which a thermometer could be used to show that heat energy is being transferred.", "stove is used to boil water in a pan", "thermometer could be used")
    Classifier.run("a claws for obtaining food is correctly paired with its function.", "claws", "obtaining food")
  }
}