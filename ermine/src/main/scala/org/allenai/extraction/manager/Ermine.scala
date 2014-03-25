package org.allenai.extraction.manager

import akka.actor.ActorSystem
import akka.io.IO
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.httpx.marshalling.BasicMarshallers
import spray.util._

import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{ Failure, Success }

import java.io.File

/** Main app to run extractions. */
object Ermine {
  def main(args: Array[String]): Unit = {
    // Load up a config file.
    val config = ConfigFactory.load()
    // TODO(jkinkead): Allow a user-specified workflow to run.
    val workflowName = "default"

    // TODO(jkinkead): Validate the input.
    val workflow = ExtractionWorkflow.fromConfig(
      config.getConfig("ermine.workflows").getConfig(workflowName))

    // Implicit actor system for sendReceive.
    implicit val system = ActorSystem("ermine-requests")
    // Execution context for futures.
    import system.dispatcher

    val pipeline = sendReceive ~> unmarshal[String]

    val infile = new File(workflow.filename)
    if (!infile.canRead()) {
      // TODO(jkinkead): Better error handling.
      println("bad infile: " + infile)
    } else {
      println("good infile: " + infile)
      val sentences = Source.fromFile(infile).getLines()
      // TODO(jkinkead): This sends requests in serial, since the prolog server
      // wasn't handling parallel requests well. This could be revisited.
      val results: Iterator[String] = for (sentence <- sentences) yield {
        println("sending " + sentence)
        val request = pipeline {
          Post(workflow.extractorUrl, sentence)
        }
        val result = Await.result(request, 10.second)
        println("result = " + result)
        result
      }
      // Force lazy iterator above to execute.
      println("results=" + results.size)
    }

    // Close any pending requests, and shut down the actor system.
    IO(Http).ask(Http.CloseAll)(10.second).await
    system.shutdown()
  }
}
