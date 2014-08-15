package org.allenai.example

import akka.event.LoggingAdapter
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing.HttpServiceActor
import scala.concurrent._
import scala.collection.mutable.Seq

trait ApiRoute1 extends SprayJsonSupport { self: HttpServiceActor =>

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
    def runInit(qid: String, path: String): Future[Response] = {
      val inputpath = "/Users/qingqingcai/Documents/java/workspace/Hackthon/data/query-urls-sens-cleaned" + "/" + qid + ".txt"
      println(inputpath)
      var qidList: List[String] = List()
      var senList: List[String] = List()
      for (line <- scala.io.Source.fromFile(inputpath).getLines) {
        val arr = line.split("\t")
        val que = arr(2)
        val sen = arr(6)
        qidList = qidList ::: List(que)
        senList = senList ::: List(sen)
      }
      Future(Response(qidList, senList))
    }

    def runClassifier(qid: String, path: String): Future[Response] = {
      val inputpath = "/Users/qingqingcai/Documents/java/workspace/Hackthon/data/query-urls-sens-classifier" + "/" + qid + ".txt"
      println(inputpath)
      var qidList: List[String] = List()
      var senList: List[String] = List()
      for (line <- scala.io.Source.fromFile(inputpath).getLines) {
        val arr = line.split("\t")
        val que = arr(2)
        val sen = arr(6)
        qidList = qidList ::: List(que)
        senList = senList ::: List(sen)
      }
      Future(Response(qidList, senList))
    }

    // API data transfer object
    // Note that the field name matches the 'text' field name
    // in the app-controller.js' scope.submit object.
    case class Submit(qid: String, path: String)
    implicit val submitFormat = jsonFormat2(Submit.apply)

    // format: OFF
    val route =
      path("submitinit") {
        post {
          entity(as[Submit]) { submit =>
          	complete{
          		Future(runInit(submit.qid, submit.path))
          	}
          }
        }
      } ~ 
      path("submitclassifier") {
        post {
          entity(as[Submit]) { submit =>
          	complete{
          		Future(runClassifier(submit.qid, submit.path))
          	}
          }
        }
      }
    // format: ON

  }
}