//package org.allenai.extraction.demo
//
//import org.allenai.common.Resource.using
//import org.allenai.common.Logging
//import org.allenai.extraction.api.definition.DefinitionDBReader
//import org.allenai.extraction.api.JsonProtocol.{ PipelineRequest, PipelineResponse }
//
//import akka.actor._
//import com.typesafe.config.Config
//import com.typesafe.config.ConfigFactory
//import com.typesafe.config.ConfigObject
//import com.typesafe.config.ConfigRenderOptions
//import de.l3s.boilerpipe.extractors.ArticleExtractor
//import dispatch.Defaults._
//import dispatch.{ Future => _, _ }
//import spray.http.StatusCodes._
//import spray.http._
//import spray.httpx.SprayJsonSupport
//import spray.json.DefaultJsonProtocol._
//import spray.json._
//import spray.routing._
//
//import java.io.File
//import java.io.StringWriter
//import java.net.URL
//import scala.collection.JavaConverters._
//import scala.concurrent._
//import scala.concurrent.duration._
//import scala.io.Source
//import scala.util.{ Try, Success, Failure }
//import scala.util.control.NonFatal
//
///** Class to handle various routes for the General demo and Otter demo.
//  * @param staticContentRoot Path of the directory with the required html and related resources.
//  * @param defaultContent Home page for the demo: different values for the General and the Otter demos.
//  * Defaults to the one for the General demo.
//  */
//class RelationDemo(val name:String) extends Directives with SprayJsonSupport with Logging {
//  def apply(section: String): Future[String] = {
//    val svc = dispatch.url(url.toString) << section
//    Http(svc OK as.String)
//  }
//}
//
//class SimpleExtractor(url: URL) extends Extractor(url) {
//  override def apply(sentence: String): Future[String] = {
//    val svc = dispatch.url(url.toString) << sentence
//    Http(svc OK as.String)
//  }
//}
//
//case class ToolResponse(texts: Seq[String], base64Images: Seq[String])
//object ToolResponse {
//  implicit val toolResponseJsonFormat = jsonFormat2(ToolResponse.apply)
//}
