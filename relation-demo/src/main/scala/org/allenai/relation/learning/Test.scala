//package org.allenai.relation.learning
//
//import java.io.File
//
//object Test {
//  def main(args: Array[String]) = {
//    //    println("\nTest1: file.separator")
//    //    val configArffTrain = "arff" + File.separator + "train.arff"
//    //    println(configArffTrain)
//    //    
//    //    println("\nTest2: list.map")
//    //    def compfea(s: String):List[Integer] = {return List(s.length(), s.length()-10)}
//    //    val list = List("monday", "tuesday", "wednesday")
//    //    println(list.map{p => (p, compfea(p))}.toMap)	//list.map{...}.toMap
//    //    
//    //    println("\nTest3: caseclass")    
//    //    SentenceDisrel.fromTrainingFile("/Users/qingqingcai/Documents/Aristo/extraction-new/data/weka/sentence-disrel-labeled.txt", 1).foreach(println)
//
//    //    println("\nTest4: features")
//    //    var features = Seq[Double]()
//    //    println(features)
//    //    features :+= 0.99
//    //    println(features)
//    //    features :+= 0.12
//    //    println(features)
//
//    //    println("\nTest5: split")
//    //    val sentence = "this information in intended as a tool to not only provide more concrete information as a tool before ."
//    //    val tokens = sentence.split("\\s+").toList
//    //    println("tokens: = " + tokens)
//    //    val arg1 = "a tool"
//    //    val arg1tokens = arg1.split("\\s+").toList
//    //    
//    //    println("\nTest6: list.flatten")
//    //    val list:List[String] = List("an example of", "called as", "a way to", "include", "such as")
//    //    var newlist:List[String] = List()
//    //    list.foreach(p => p.split("\\s+").foreach(q => newlist=newlist:::List(q)))
//    //    println(newlist)
//
//    import FeatureWrapper._
//    import org.allenai.ari.solvers.utils.Tokenizer
//    println("\nTest7: set.intersect")
//    val sentence = "this information in intended as a tool to not only provide more concrete information as a tool before ."
//    val arg1 = "provide more not only information hahahahah"
//    val sentenceSet = Tokenizer.toKeywords(sentence).toSet
//    val arg1Set = Tokenizer.toKeywords(arg1).toSet
//    println(sentenceSet)
//    println(sentenceSet.intersect(arg1Set))
//    println(arg1Set)
//    println(arg1Set.mkString(" "))
//
//    //    println("\nTest8: entailment")
//    //    import org.allenai.ari.solvers.inference.matching.{ EntailmentWrapper, EntailmentService }
//    //    val wordnetEntailmentService: EntailmentService = {
//    //      val wordnetEntailmentUrl = "http://entailment.dev.allenai.org:8191/api/entails"
//    //      val wrapper = new EntailmentWrapper(wordnetEntailmentUrl)
//    //      wrapper.CachedEntails
//    //    }
//    //
//    //    def wordnetEntailment(text: String, hypothesis: String) =
//    //      wordnetEntailmentService(text, hypothesis) map { _.confidence } getOrElse 0d
//    //
//    //    val word2vecEntailmentService: EntailmentService = {
//    //      val word2vecEntailmentUrl = "???"
//    //      val wrapper = new EntailmentWrapper(word2vecEntailmentUrl)
//    //      wrapper.CachedEntails
//    //    }
//    //      
//    //    println(wordnetEntailment("is considered to be one of ", "is thought as"))
//  }
//}