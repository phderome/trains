package com.deromefintech.trains

import akka.actor.{ActorSystem, Props}

import scala.concurrent.duration._
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.util.Timeout

import scala.io.StdIn

object TrainWebServer {
  implicit val system = ActorSystem("Trains")
  val webTrainActor = system.actorOf(Props[TrainActor], TrainActor.WebName)
  implicit val materializer = ActorMaterializer()

  // needed for the future map/flatmap in the end
  implicit val executionContext = system.dispatcher
  implicit val timeout: Timeout = Timeout(5 seconds)

  def main(args: Array[String]): Unit = {
    val trainRoutes = new TrainRoutes(webTrainActor)
    val bindingFuture = Http().bindAndHandle(trainRoutes.routes, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }

}
