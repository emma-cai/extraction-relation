package org.allenai.example

import akka.event.LoggingAdapter
import akka.actor.ActorLogging
import spray.httpx.SprayJsonSupport
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing.HttpServiceActor
import scala.concurrent._
import scala.collection.mutable.Seq
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

trait ApiRoute extends SprayJsonSupport { self: HttpServiceActor with ActorLogging =>

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

    def savePositiveData(qid: String, positive: Array[String]) = {
      val outputfile = new File("/Users/qingqingcai/Documents/java/workspace/Hackthon/data/websaved.txt")
      val fw = new BufferedWriter(new FileWriter(outputfile, true))

      positive.foreach {
        case l =>
          {
            fw.write(l)
            fw.newLine()
          }
          fw.newLine()
      }
      fw.flush()
      fw.close()
    }

    // API data transfer object
    // Note that the field name matches the 'text' field name
    // in the app-controller.js' scope.submit object.
    case class Submit(qid: String, path: String)
    implicit val submitFormat = jsonFormat2(Submit.apply)
    //    case class Save(qid:String, positive:Array[String])
    //    object MyJsonProtocol extends DefaultJsonProtocol {
    //      implicit object SaveJsonFormat extends RootJsonFormat[Save] {
    //        def write(save: Save): JsValue = {
    //          JsObject(
    //            "input" -> save.qid.toJson,
    //            "processors" -> save.positive.toSeq.toJson)
    //          }
    //      }
    //    }

    case class Judgesubmit(qid: String, positive: Array[String])
    implicit val judgesubmitFormat = jsonFormat2(Judgesubmit.apply)
    
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
      } ~ 
      path("submitjudge") {
        post {
          entity(as[Judgesubmit]) { judgesubmit =>
            log.info("got judge submit: " + judgesubmit.toString)
          	complete{
          		"todo"
         //      savePositiveData(judgesubmit.qid, judgesubmit.positive)
               savePositiveData(judgesubmit.qid, judgesubmit.positive)
               "OK"
          	}
          }
        }
      }
    //      ~
    //      path("savepositive") {
    //        post {
    //          
    //          }
    //        }
    //      }
    // format: ON

  }
}