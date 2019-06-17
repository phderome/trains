package com.deromefintech.trains

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import akka.pattern._
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.{Directive, Route}
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import com.deromefintech.trains.domain.model._
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}

class TrainRoutes(trainActor: ActorRef)(implicit val ec:ExecutionContext, timeout: Timeout) extends TrainJsonProtocol {
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

  def routes(implicit system: ActorSystem, ec: ExecutionContext, mater: Materializer): Route =

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
      (put | post) {  // example to show that we can convert PUT requests to POST requests if we need to make them equivalent.
        mapRequest(_.copy(method = HttpMethods.POST)) {
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
              // example taken from TrainApp, allowing same queries to be made
              // AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7
              // curl -H "Content-Type: application/json" -X POST -d '{"edgeCount":9,
              // "weightedEdges":[{"edge":{"s":"A", "t":"B"},"w":5},{"edge":{"s":"B", "t":"C"},"w":4},{"edge":{"s":"C", "t":"D"},"w":8},
              // {"edge":{"s":"D", "t":"C"},"w":8},{"edge":{"s":"D", "t":"E"},"w":6},{"edge":{"s":"A", "t":"D"},"w":5},
              // {"edge":{"s":"C", "t":"E"},"w":2},{"edge":{"s":"E", "t":"B"},"w":3},{"edge":{"s":"A", "t":"E"},"w":7}
              // ]}' http://localhost:8080/create-network
              entity(as[NetworkCreate]) { e =>
                respond(
                  (trainActor ? e).mapTo[Either[Rejected, NetworkCreated]]
                )
              }
            }
        }
      } ~
      (path("delete-edge") & delete) {
        // example
        // curl -H "Content-Type: application/json" -X DELETE -d '{"edge":{"edge":{"s":"A", "t":"B"},"w":2}}'
        // http://localhost:8080/delete-edge
        entity(as[DeleteEdge]) { e =>
          respond(
            (trainActor ? e).mapTo[Either[Rejected, EdgeDeleted]]
          )
        }
      }

}
