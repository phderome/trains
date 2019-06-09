package com.deromefintech.trains

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import TrainActor._
import com.deromefintech.trains.domain.model._
class TrainActor() extends Actor with ActorLogging {
  import TrainService._

  var trainGraph: Option[TrainGraph] = None

  def receive: Receive = create

  def create: Receive = {
    case NetworkCreate(edgeCount, weightedEdges) ⇒
      handleNetworkCreate(edgeCount, weightedEdges).foreach { g =>
        trainGraph = Some(g)
        context.become(created)
      }

    case DeleteEdge(edge) ⇒
      rejectOffline(sender(), DeleteEdge(edge))

    case UpdateEdge(weightedEdge, old) ⇒
      rejectOffline(sender(), UpdateEdge(weightedEdge, old))

    case q: Query ⇒ rejectQuery(q)
  }

  private def rejectOffline(ref: ActorRef, c: Command): Unit = {
    val reject = RejectedCommand(c, s"invalid command $c as train network is offline")
    sender() ! reject
    log.warning(reject.toString)
  }

  private def rejectOnline(ref: ActorRef, c: Command): Unit = {
    val reject = RejectedCommand(c, s"invalid command $c")
    sender() ! reject
    log.warning(reject.toString)
  }

  def created: Receive = {
    case NetworkCreate(edgeCount, weightedEdges) ⇒
      handleNetworkCreate(edgeCount, weightedEdges).foreach { g =>
        trainGraph = Some(g)
      } // on failure, we simply keep former graph, once created, always created.

    case DeleteEdge(e) ⇒
      trainGraph
        .map(_.service)
        .foreach { service =>
        service.deleteEdge(e) match {
          case None =>
            val cmd = DeleteEdge(e)
            rejectOnline(sender(), cmd)
          case Some(newService) =>
            val deleted = EdgeDeleted(e)
            val msg = s"train network deleted edge $deleted"
            trainGraph = Some(TrainGraph(newService))
            sender() ! deleted
            log.info(msg)
        }
      }

    case UpdateEdge(weightedEdge, old) ⇒
      trainGraph
        .map(_.service)
        .foreach { s =>
          s.updateEdge(weightedEdge, old) match {
            case None =>
              val cmd = UpdateEdge(weightedEdge, old)
              rejectOnline(sender(), cmd)
            case Some(newService) =>
              val updated = EdgeUpdated(weightedEdge)
              val msg = s"train network updated edge $updated"
              trainGraph = Some(TrainGraph(newService))
              sender() ! updated
              log.info(msg)
          }
        }

    case q: Query ⇒
      trainGraph
        .map(_.service) match {
        case Some(service) =>
          val result = handleQuery(service, q)
          val msg = s"${q.show}: $result"
          log.info(msg)
          sender() ! AcceptedQuery(q, msg)
        case None =>
          log.error("created state has unexpected unavailable service") // odd situation, not expected, but we can reject nonetheless
          rejectQuery(q)
      }

  }

  def handleQuery(service: TrainService, q: Query): String=
    q match {
      case Distance(walk) =>
        service.getDistance(walk).show

      case WalksMaxHopsSelectLast(s, t, limit) =>
        service.findWalksMaxHopsSelectLast(s, t, limit).show

      case WalksExactSelectLast(s, t, limit) =>
        service.findWalksExactSelectLast(s, t, limit).show

      case ShortestRoute(s, t) =>
        service.shortestRoute(s, t).show

      case WalksWithinDistanceSelectLast(s, t, limit) =>
        service.exploreWalksWithinDistanceSelectLast(s, t, limit).show
    }

  def rejectQuery(q: Query): Unit = {
    val reject = RejectedQuery(q, s"cannot satisfy query ${q.show} as train network is offline")
    sender() ! reject
    log.warning(reject.toString)
  }

  def handleNetworkCreate(edgeCount: Int, weightedEdges: List[RawWeightedEdge]): Option[TrainGraph] = {
    val command = NetworkCreate(edgeCount, weightedEdges)
    TrainService.createRoutes(edgeCount, weightedEdges).map(TrainService(_)) match {
      case None =>
        val reject = RejectedCommand(command, s"invalid command $command")
        sender() ! reject
        log.warning(reject.toString)
        None
      case Some(service) =>
        val created = NetworkCreated(edgeCount, weightedEdges)
        val msg = s"created train network $created"
        sender() ! created
        log.info(msg)
        Some(new TrainGraph(service))
    }

  }
}

object TrainActor {

  final val Name = "train-actor"

  def apply(): Props = Props(new TrainActor())

  protected sealed trait State
  protected case object Offline extends State
  protected case object Active extends State

  protected sealed trait Data
  protected case object Uninitialized extends Data
  protected final case class TrainGraph(service: TrainService) extends Data

  // Supported Messages are from trains.domain.model

  // for testing.
  def getSampleQueries: List[Query] = {
    /*
     The distance of the route A-B-C.
     The distance of the route A-D.
     The distance of the route A-D-C.
     The distance of the route A-E-B-C-D.
     The distance of the route A-E-D.
    */
    val distanceQueries = List("ABC", "AD", "ADC", "AEBCD", "AED").map(Distance(_))

    /*
      The number of trips starting at C and ending at C with a maximum of 3
      stops.  In the sample data, there are two such trips: C-D-C (2
      stops). and C-E-B-C (3 stops).
    */
    val maxHopsSelectLastQueries = WalksMaxHopsSelectLast(s = 'C', t = 'C', limit = 3) :: Nil

    /*
      The number of trips starting at A and ending at C with exactly 4 stops.
        In the sample data, there are three such trips: A to C (via B,C,D); A
        to C (via D,C,D); and A to C (via D,E,B).
    */
    val exactSelectLastQueries = WalksExactSelectLast(s = 'A', t = 'C', limit = 4) :: Nil

    /*
      The length of the shortestRoute route (in terms of distance to travel) from A
      to C and then B to B
    */
    val shortestRouteQueries = List(ShortestRoute(s = 'A', t = 'C'), ShortestRoute(s = 'B', t = 'B'))

    /*
      The number of different routes from C to C with a distance of less than
      30.  In the sample data, the trips are: CDC, CEBC, CEBCDC, CDCEBC, CDEBC,
      CEBCEBC, CEBCEBCEBC. So 7 of them.
    */
    val withinDistanceSelectLastQueries = WalksWithinDistanceSelectLast(s = 'C', t = 'C', limit = 30) :: Nil

    distanceQueries ++
      maxHopsSelectLastQueries ++
      exactSelectLastQueries ++
      shortestRouteQueries ++
      withinDistanceSelectLastQueries
  }
}
