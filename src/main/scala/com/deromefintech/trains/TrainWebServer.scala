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
import domain.model._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Try
import cats.implicits._

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

  def submitQuery(q: Option[Query], ref: ActorRef, error: => String): Route = {
    val response: Future[Either[Rejected, Accepted]] =
      q match {
        case Some(x) => (ref ? x).mapTo[Either[Rejected, Accepted]]
        case None => Future.failed(new RuntimeException(error))
      }

    onSuccess(response) {
      case Right(e) => complete(e)
      case _ => complete(StatusCodes.NotFound)
    }
  }

  def route(trainActor: ActorRef): Route =

    pathPrefix("distance") {  // curl "http://localhost:8080/distance?src=A&dest=C"
      parameters('src, 'dest) { (s, t) =>
        get {
          lazy val badInput = s"invalid s($s)--t($t)"
          val distance = (s.headOption, t.headOption)
            .mapN((s, t) => Distance(List(s, t).mkString))
          submitQuery(distance, trainActor, badInput)
        }
      }
    } ~
      pathPrefix("shortest") {  // curl "http://localhost:8080/shortest?src=A&dest=C"
        parameters('src, 'dest) { (s, t) =>
          get {
            lazy val badInput = s"invalid s($s)--t($t)"
            val shortRoute = (s.headOption, t.headOption)
              .mapN((s, t) => ShortestRoute(s, t))

            submitQuery(shortRoute, trainActor, badInput)

          }
        }
      } ~
      pathPrefix("walksMaxHopsSelectLast") {  // curl "http://localhost:8080/walksMaxHopsSelectLast?src=A&dest=C&limit=5"
        parameters('src, 'dest, 'limit) { (s, t, limit) =>
          get {
            lazy val badInput = s"invalid s($s)--t($t) limit($limit)"
            val walk = (s.headOption, t.headOption, Try(limit.toInt).toOption)
              .mapN((s, t, lim) => WalksMaxHopsSelectLast(s, t, lim))

            submitQuery(walk, trainActor, badInput)
          }
        }
      } ~
      pathPrefix("walksExactSelectLast") {  // curl "http://localhost:8080/walksExactSelectLast?src=A&dest=C&limit=5"
        parameters('src, 'dest, 'limit) { (s, t, limit) =>
          get {
            lazy val badInput = s"invalid s($s)--t($t) limit($limit)"
            val walk = (s.headOption, t.headOption, Try(limit.toInt).toOption)
              .mapN((s, t, lim) => WalksExactSelectLast(s, t, lim))

            submitQuery(walk, trainActor, badInput)
          }
        }
      } ~
      pathPrefix("walksWithinDistanceSelectLast") {
        // curl "http://localhost:8080/walksWithinDistanceSelectLast?src=A&dest=C&limit=5"
        parameters('src, 'dest, 'limit) { (s, t, limit) =>
          get {
            lazy val badInput = s"invalid s($s)--t($t) limit($limit)"
            val walks = (s.headOption, t.headOption, Try(limit.toInt).toOption)
              .mapN((s, t, lim) => WalksWithinDistanceSelectLast(s, t, lim))

            submitQuery(walks, trainActor, badInput)

          }
        }
      } ~
        post {
          path("delete-edge") {
            // example
            // curl -H "Content-Type: application/json" -X POST -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2}}'
            // http://localhost:8080/delete-edge
            entity(as[DeleteEdge]) { e =>
              val edgeDeleted: Future[Either[Rejected, EdgeDeleted]] =
                (trainActor ? e).mapTo[Either[Rejected, EdgeDeleted]]
              onSuccess(edgeDeleted) {
                case Right(e) => complete(e)
                case _ => complete(StatusCodes.NotFound)
              }
            }
          }
        } ~
        post {
          path("update-edge") {
            // example
            // curl -H "Content-Type: application/json" -X POST -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2},"formerWeight":0}'
            // http://localhost:8080/update-edge
            entity(as[UpdateEdge]) { e =>
              val edgeUpdated: Future[Either[Rejected, EdgeUpdated]] =
                (trainActor ? e).mapTo[Either[Rejected, EdgeUpdated]]
              onSuccess(edgeUpdated) {
                case Right(e) => complete(e)
                case _ => complete(StatusCodes.NotFound)
              }
            }
          }
        } ~
        post {
          path("create-network") {
            // example
            // curl -H "Content-Type: application/json" -X POST -d '{"edgeCount":1,
            // "weightedEdges":[{"edge":{"s":"A", "t":"B"},"w":2}]}' http://localhost:8080/create-network
            entity(as[NetworkCreate]) { e =>
              val saved: Future[Either[Rejected, NetworkCreated]] =
                (trainActor ? e).mapTo[Either[Rejected, NetworkCreated]]
              onSuccess(saved) {
                case Right(e) => complete(e)
                case _ => complete(StatusCodes.NotFound)
              }
            }
          }
        }

      def main(args: Array[String]): Unit = {

        val bindingFuture = Http().bindAndHandle(route(webTrainActor), "localhost", 8080)
        println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
        StdIn.readLine() // let it run until user presses return
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ ⇒ system.terminate()) // and shutdown when done

      }

}
