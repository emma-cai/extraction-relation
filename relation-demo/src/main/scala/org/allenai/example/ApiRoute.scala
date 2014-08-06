//package org.allenai.example
//
//import akka.event.LoggingAdapter
//import spray.httpx.SprayJsonSupport
//import spray.json._
//import spray.json.DefaultJsonProtocol._
//import spray.routing.HttpServiceActor
//import scala.concurrent._
//import scala.collection.mutable.Seq
//
//
//trait ApiRoute extends SprayJsonSupport { self: HttpServiceActor =>
//
//  import context._
//  import org.allenai.relation.util._
//  import scala.collection.mutable.Map
//  
//  object apiRoute {
//    
////    case class Response(kp: String, sen: String)
////      implicit object ResponseWriter extends RootJsonWriter[Response] {
////      import spray.json._
////      def write(response: Response): JsValue = {
////        JsObject(
////          "kp" -> response.kp.toJson,
////          "sen" -> response.sen.toJson
////          )
////      }
////    }
////    
////    import scala.concurrent.Future
////    def test(query:String):Future[Response] = {
////      val reverbSearch:Searching = new Searching()
////      val searchres = reverbSearch.runSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", "\""+query+"\"", "kp", List("kp", "sen"), 1000)
////
////      Future(Response("key phrase1", "sentence 1"))
////    }
//    
//    
//    case class Response(kpset: List[String], senset: List[String])
//      implicit object ResponseWriter extends RootJsonWriter[Response] {
//      import spray.json._
//      def write(response: Response): JsValue = {
//        JsObject(
//          "kp" -> response.kpset.toJson,
//          "sen" -> response.senset.toJson
//          )
//      }
//    }
//    
//    import scala.concurrent.Future
//    def test(query:String):Future[Response] = {
//      val reverbSearch:Searching = new Searching()
//      val searchres = reverbSearch.runSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", "\""+query+"\"", "kp", List("kp", "sen"), 1000)
//      var kpset:List[String] = List()
//      var senset:List[String] = List()
//      searchres.foreach {
//        case per => {
//          kpset = kpset :+ per(0)
//          senset = senset :+ per(1)
//        }
//      }
//      Future(Response(kpset, senset))
//    }
//
//    
//   
//    
//    
//    // API data transfer object
//    // Note that the field name matches the 'text' field name
//    // in the app-controller.js' scope.submit object.
//    case class Submit(text: String)
//    implicit val submitFormat = jsonFormat1(Submit.apply)
//
//    // format: OFF
//    val route =
//      path("submit") {
//        post {
//          entity(as[Submit]) { submit =>
//          	import spray.json.DefaultJsonProtocol._
//          	import spray.json._
//          	import spray.httpx.marshalling.ToResponseMarshaller._
//          	
//            complete{
//          		Future(test(submit.text))
//          	}
//          }
//        }
//      }
//    // format: ON
//  }
//    
//}


package org.allenai.example

import akka.event.LoggingAdapter
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing.HttpServiceActor
import scala.concurrent._
import scala.collection.mutable.Seq


trait ApiRoute extends SprayJsonSupport { self: HttpServiceActor =>

  import context._
  import scala.collection.mutable.Map
  import org.allenai.relation.api._
 
  
  object apiRoute {
    
    
    case class Response(kpset: List[String], senset: List[String]) {
      def toTuples = kpset.zip(senset)
    }
    
    implicit object ResponseWriter extends RootJsonWriter[Response] {
      import spray.json._
      def write(response: Response): JsValue = response.toTuples.toJson
    }
    
    import scala.concurrent.Future
    def runInstanceSearch(disrel:String, query:String):Future[Response] = {
      val insSearch:InstanceSearching = new InstanceSearching()
      val searchres = insSearch.insSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", 
          disrel, "\""+query+"\"")
      var kpset:List[String] = List()
      var senset:List[String] = List()
      searchres.foreach {
        case per => {
          kpset = kpset :+ per(0)
          senset = senset :+ per(1)
        }
      }
      Future(Response(kpset, senset))
    }
    
    
    
    def runSentenceSearch(disrel:String, arg1:String, arg2:String):Future[Response] = {
      val search:SentenceSearching = new SentenceSearching()
//      val searchres = search.runSearch("/Users/qingqingcai/Documents/Data/Reverb/Index", 
//          "\""+arg1+"\"", "\""+arg2+"\"", "arg1", "arg2", List("kp", "sen"), 100)
      val searchres = search.senSearch("/Users/qingqingcai/Documents/Aristo/extraction-new/data/disrel_tuples_v2", disrel, arg1, arg2)
      
      var kpset:List[String] = List()
      var senset:List[String] = List()
      searchres.foreach {
        case per => {
          kpset = kpset :+ per(0)
          senset = senset :+ per(1)
        }
      }
      Future(Response(kpset, senset))
    }
    
    
    
    // API data transfer object
    // Note that the field name matches the 'text' field name
    // in the app-controller.js' scope.submit object.
    case class Submit(disrel: String, kp:String)
    implicit val submitFormat = jsonFormat2(Submit.apply)

    case class ArgSubmit(disrel:String, arg1:String, arg2:String)
    implicit val argsubmitFormat = jsonFormat3(ArgSubmit.apply)
    // format: OFF
    val route =
      path("submit") {
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
      }
    // format: ON
    
  }
    
}