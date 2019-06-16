package com.deromefintech.trains

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern._

import scala.concurrent.duration._
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.{Directive, Route}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import domain.model.{Query, _}
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future
import scala.io.StdIn
import cats.implicits._

trait DomainMarshallers {
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
}

object TrainWebServer extends DomainMarshallers {
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

  // needed to run the route
  implicit val system = ActorSystem("Trains")
  val webTrainActor = system.actorOf(Props[TrainActor], TrainActor.WebName)
  implicit val materializer = ActorMaterializer()

  // needed for the future map/flatmap in the end and Future.apply in fetchItem and saveOrder
  implicit val executionContext = system.dispatcher
  implicit val timeout: Timeout = Timeout(5 seconds)

  def buildEdgeQuery(s: String, t: String, f: (Char, Char) => Query): Option[Query] =
    (s.headOption, t.headOption)
      .mapN((s, t) => f(s, t))

  private def badEdgeInput(s: String, t: String): String = s"invalid s($s)--t($t)"

  def buildEdgeWLimitQuery(s: String,
                           t: String,
                           limit: Int,
                           f: (Char, Char, Int) => Query): Option[Query] =
    (s.headOption, t.headOption)
      .mapN((s, t) => f(s, t, limit))

  private def badEdgeWLimitInput(s: String, t: String, limit: Int): String = s"invalid s($s)--t($t) limit($limit)"

  def submitQuery(q: Option[Query], ref: ActorRef, error: => String): Route = {
    val response: Future[Either[Rejected, Accepted]] =
      q match {
        case Some(x) => (ref ? x).mapTo[Either[Rejected, Accepted]]
        case None => Future.failed(new RuntimeException(error))
      }

    onSuccess(response) {
      case Right(e) => complete(e)
      case Left(rej) => failWith(new RuntimeException(rej.msg))
    }
  }

  private def respond[T <: Event](done: Future[Either[Rejected, T]])(implicit ev: ToResponseMarshaller[T]): Route =
    onSuccess(done) {
      case Right(e) => complete(e)
      case Left(rej) => failWith(new RuntimeException(rej.msg))
    }

  private def edgeWLimitDirective(segment: String): Directive[(String, String, Int)] =
    path(segment) & get & parameters('src, 'dest, 'limit.as[Int])

  private def edgeDirective(segment: String): Directive[(String, String)] =
    path(segment) & get & parameters('src, 'dest)

  private def postDirective(segment: String): Directive[Unit] =
    path(segment) & post

  def route(trainActor: ActorRef): Route =

    edgeDirective("distance")  { (s, t) =>
        // curl "http://localhost:8080/distance?src=A&dest=B"
        submitQuery(
          buildEdgeQuery(s, t, (a, b) => Distance(List(a, b).mkString)),
          trainActor,
          badEdgeInput(s, t)
        )
    } ~
    edgeDirective("shortest")  { (s, t) =>
      // curl "http://localhost:8080/shortest?src=A&dest=B"
      submitQuery(
        buildEdgeQuery(s, t, ShortestRoute(_, _)),
        trainActor,
        badEdgeInput(s, t)
      )
    } ~
    edgeWLimitDirective("walksMaxHopsSelectLast")  { (s, t, limit) =>
      // curl "http://localhost:8080/walksMaxHopsSelectLast?src=A&dest=B&limit=5"
      val walk = buildEdgeWLimitQuery(s, t, limit, WalksMaxHopsSelectLast)
      submitQuery(walk, trainActor, badEdgeWLimitInput(s, t, limit))
    } ~
    edgeWLimitDirective("walksExactSelectLast") { (s, t, limit) =>
      // curl "http://localhost:8080/walksExactSelectLast?src=A&dest=B&limit=5"
      val walk = buildEdgeWLimitQuery(s, t, limit, WalksExactSelectLast)
      submitQuery(walk, trainActor, badEdgeWLimitInput(s, t, limit))
    } ~
    edgeWLimitDirective("walksWithinDistanceSelectLast") { (s, t, limit) =>
      // curl "http://localhost:8080/walksWithinDistanceSelectLast?src=A&dest=B&limit=5"
      val walks = buildEdgeWLimitQuery(s, t, limit, WalksWithinDistanceSelectLast)
      submitQuery(walks, trainActor, badEdgeWLimitInput(s, t, limit))
    } ~
    postDirective("delete-edge") {
      // example
      // curl -H "Content-Type: application/json" -X POST -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2}}'
      // http://localhost:8080/delete-edge
      entity(as[DeleteEdge]) { e =>
        respond(
          (trainActor ? e).mapTo[Either[Rejected, EdgeDeleted]]
        )
      }
    } ~
    postDirective("update-edge")  {
      // example
      // curl -H "Content-Type: application/json" -X POST -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2},"formerWeight":0}'
      // http://localhost:8080/update-edge
      entity(as[UpdateEdge]) { e =>
        respond(
          (trainActor ? e).mapTo[Either[Rejected, EdgeUpdated]]
        )
      }
    } ~
    postDirective("create-network") {
      // example
      // curl -H "Content-Type: application/json" -X POST -d '{"edgeCount":1,
      // "weightedEdges":[{"edge":{"s":"A", "t":"B"},"w":2}]}' http://localhost:8080/create-network
      entity(as[NetworkCreate]) { e =>
        respond(
          (trainActor ? e).mapTo[Either[Rejected, NetworkCreated]]
        )
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
