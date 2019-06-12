package com.deromefintech.trains

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern._

import scala.concurrent.duration._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.deromefintech.trains.domain.model._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future
import scala.io.StdIn

object TrainWebServer {

  // needed to run the route
  implicit val system = ActorSystem("Trains")
  val webTrainActor = system.actorOf(Props[TrainActor], TrainActor.WebName)
  implicit val materializer = ActorMaterializer()

  // needed for the future map/flatmap in the end and Future.apply in fetchItem and saveOrder
  implicit val executionContext = system.dispatcher
  implicit val timeout: Timeout = Timeout(5 seconds)

  // formats for unmarshalling and marshalling
  implicit val rawEdge1Format = jsonFormat2(RawEdge)
  implicit val rawWeightedEdge1Format = jsonFormat2(RawWeightedEdge)
  implicit val networkCreate1Format = jsonFormat2(NetworkCreate)
  implicit val deleteEdge1Format = jsonFormat1(DeleteEdge)
  implicit val updateEdge1Format = jsonFormat2(UpdateEdge)

  implicit val networkCreatedFormat = jsonFormat2(NetworkCreated)
  implicit val edgeDeleteFormat = jsonFormat1(EdgeDeleted)
  implicit val edgeUpdatedFormat = jsonFormat2(EdgeUpdated)

  implicit val distanceFormat = jsonFormat1(Distance)

  implicit val walksMaxHopsSelectLastFormat = jsonFormat3(WalksMaxHopsSelectLast)

  implicit val walksExactSelectLastFormat = jsonFormat3(WalksExactSelectLast)
  implicit val shortestRouteFormat = jsonFormat2(ShortestRoute)
  implicit val walksWithinDistanceSelectLastFormat = jsonFormat3(WalksWithinDistanceSelectLast)

  implicit val acceptedQueryFormat = jsonFormat1(Accepted)
  implicit val rejectedResponseFormat = jsonFormat1(Rejected)

  def route(trainActor: ActorRef): Route =

    pathPrefix("distance") {  // curl http://localhost:8080/distance/A/dest/C
      path(Segment / "dest" / Segment) { (s, t) =>
        get {
          val response: Future[Either[Accepted, Rejected]] =
            (trainActor ? Distance(List(s.head, t.head).mkString)).mapTo[Either[Accepted, Rejected]]
          onSuccess(response) {
            case Left(Accepted(result)) =>
              complete(Accepted(result))
            case _ =>
              complete(StatusCodes.NotFound)
          }
        }
      }
    } ~ // we could add all the GET end points, we get the idea.
      pathPrefix("walksMaxHopsSelectLast") {  // curl http://localhost:8080/walksMaxHopsSelectLast/A/dest/C/limit/5
        path(Segment / "dest" / Segment / "limit" / Segment) { (s, t, limit) =>
          get {
            val response: Future[Either[Accepted, Rejected]] =
              (trainActor ? (WalksMaxHopsSelectLast(s.head, t.head, limit.toInt))).mapTo[Either[Accepted, Rejected]]
            onSuccess(response) {
              case Left(Accepted(result)) =>
                complete(Accepted(result))
              case _ =>
                complete(StatusCodes.NotFound)
            }
          }
        }
      } ~
        post {
          path("delete-edge") {
            // example
            // curl -H "Content-Type: application/json" -X POST -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2}}'
            // http://localhost:8080/delete-edge
            entity(as[DeleteEdge]) { network =>
              val edgeDeleted: Future[Either[EdgeDeleted, Rejected]] =
                (trainActor ? (network)).mapTo[Either[EdgeDeleted, Rejected]]
              onSuccess(edgeDeleted) {
                case Left(EdgeDeleted(edge)) =>
                  complete(EdgeDeleted(edge))
                case _ =>
                  complete(StatusCodes.NotFound)
              }
            }
          }
        } ~
        post {
          path("update-edge") {
            // example
            // curl -H "Content-Type: application/json" -X POST -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2},"formerWeight":0}'
            // http://localhost:8080/update-edge
            entity(as[UpdateEdge]) { network =>
              val edgeUpdated: Future[Either[EdgeUpdated, Rejected]] =
                (trainActor ? (network)).mapTo[Either[EdgeUpdated, Rejected]]
              onSuccess(edgeUpdated) {
                case Left(EdgeUpdated(edge, formerWeight)) =>
                  complete(EdgeUpdated(edge, formerWeight))
                case _ =>
                  complete(StatusCodes.NotFound)
              }
            }
          }
        } ~
        post {
          path("create-network") {
            // example
            // curl -H "Content-Type: application/json" -X POST -d '{"edgeCount":1,
            // "weightedEdges":[{"edge":{"s":"A", "t":"B"},"w":2}]}' http://localhost:8080/create-network
            entity(as[NetworkCreate]) { network =>
              val saved: Future[Either[NetworkCreated, Rejected]] =
                (trainActor ? (network)).mapTo[Either[NetworkCreated, Rejected]]
              onSuccess(saved) {
                case Left(NetworkCreated(edgeCount, weightedEdges)) =>
                  complete(NetworkCreated(edgeCount, weightedEdges))
                case _ =>
                  complete(StatusCodes.NotFound)
              }
            }
          }
        }

      def main(args: Array[String]) {

        val bindingFuture = Http().bindAndHandle(route(webTrainActor), "localhost", 8080)
        println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
        StdIn.readLine() // let it run until user presses return
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ ⇒ system.terminate()) // and shutdown when done

      }

}
