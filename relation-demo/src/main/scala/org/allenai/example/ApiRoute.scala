package org.allenai.example

import akka.event.LoggingAdapter
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing.HttpServiceActor
import scala.concurrent._
import scala.collection.mutable.Seq

trait BACKApiRoute extends SprayJsonSupport { self: HttpServiceActor =>

  import context._
  import scala.collection.mutable.Map
  import org.allenai.relation.api._

  object apiRoute {

    case class Response(col1: List[String], col2: List[String]) {
      def toTuples = col1.zip(col2)
    }

    implicit object ResponseWriter extends RootJsonWriter[Response] {
      import spray.json._
      def write(response: Response): JsValue = response.toTuples.toJson
    }

    import scala.concurrent.Future
    def runInstanceSearch(disrel: String, query: String): Future[Response] = {
      val insSearch: InstanceSearching = new InstanceSearching()
      val searchres = insSearch.insSearch("/Users/qingqingcai/Documents/Data/Reverb/Index",
        disrel, "\"" + query + "\"")
      var kpset: List[String] = List()
      var senset: List[String] = List()
      searchres.foreach {
        case per => {
          kpset = kpset :+ per(0)
          senset = senset :+ per(1)
        }
      }
      Future(Response(kpset, senset))
    }

    def runSentenceSearch(disrel: String, arg1: String, arg2: String): Future[Response] = {
      val search: SentenceSearching = new SentenceSearching()
      //      val searchres = search.runSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", 
      //          "\""+arg1+"\"", "\""+arg2+"\"", "arg1", "arg2", List("kp", "sen"), 100)
      val searchres = search.senSearch("/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_v2", disrel, arg1, arg2)

      var kpset: List[String] = List()
      var senset: List[String] = List()
      searchres.foreach {
        case per => {
          kpset = kpset :+ per(0)
          senset = senset :+ per(1)
        }
      }
      Future(Response(kpset, senset))
    }

    def runClassifier(sentence: String, arg1: String, arg2: String): Future[Response] = {
      val (predictlist, confidencelist) = Classifier.run(sentence, arg1, arg2)
      Future(Response(predictlist, confidencelist))
    }

    // API data transfer object
    // Note that the field name matches the 'text' field name
    // in the app-controller.js' scope.submit object.
    case class Submit(disrel: String, kp: String)
    implicit val submitFormat = jsonFormat2(Submit.apply)

    case class ArgSubmit(disrel: String, arg1: String, arg2: String)
    implicit val argsubmitFormat = jsonFormat3(ArgSubmit.apply)
    
//    case class SaveSubmit(disrel:String, sens:Seq[String])
//    implicit val savesubmitFormat = jsonFormat2(SaveSubmit)
    // format: OFF
    
    case class ClassifierSubmit(sentence: String, arg1: String, arg2: String)
    implicit val classifiersubmit = jsonFormat3(ClassifierSubmit.apply)
    
    
    val route =
      path("submitdisrel") {
        post {
          entity(as[Submit]) { submit =>
          	complete{
          		Future(runInstanceSearch(submit.disrel, submit.kp))
          	}
          }
        }
      } ~ 
      path("submitins") {
        post {
          entity(as[ArgSubmit]) { argsubmit =>
          	complete{
          		Future(runSentenceSearch(argsubmit.disrel, argsubmit.arg1, argsubmit.arg2))
          	}
          }
        }
      } ~ 
      path("classifysentence") {
        post {
          entity(as[ClassifierSubmit]) { classifiersubmit =>
          	complete{
          		Future(runClassifier(classifiersubmit.sentence, classifiersubmit.arg1, classifiersubmit.arg2))
          	}
          }
        }
      }
    //      ~ 
    //      path("savepositive") {
    //        post {
    //          entity(as[SaveSubmit]) { savesubmit =>
    //          	complete{
    //          		Future(saveData(savesubmit.disrel, savesubmit.sens))
    //          	}
    //          }
    //        }
    //      }
    // format: ON

  }
}