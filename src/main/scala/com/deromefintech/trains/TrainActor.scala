package com.deromefintech.trains

import akka.actor.{FSM, Props}
import TrainActor._
class TrainActor() extends FSM[State, Data] {
  import Messages._
  startWith(Offline, Uninitialized)

  when(Offline) {
    case Event(NetworkCreateCommand(edgeCount, weightedEdges), _) ⇒
      handleNetworkCreateCommand(edgeCount, weightedEdges)

    case Event(q: Query, _) ⇒
      rejectQuery(q)
  }

  onTransition {
    case Active -> Offline ⇒ () // nothing to do
  }

  when(Active) {
    case Event(NetworkCreateCommand(edgeCount, weightedEdges), _) ⇒
      handleNetworkCreateCommand(edgeCount, weightedEdges)

    case Event(q: Query, TrainGraph(service)) ⇒
      val result = handleQuery(service, q)
      val msg = s"${q.show}: $result"
      log.info(msg)
      sender() ! AcceptedQuery(q, msg)
      stay()

    case Event(q: Query, Uninitialized) ⇒
      rejectQuery(q)
  }

  whenUnhandled {
    case Event(e, s) ⇒
      log.warning("received unhandled request {} in state {}/{}", e, stateName, s)
      stay
  }

  initialize()

  implicit class ShowOptionalDistance(opt: Option[Int]) {
    def show: String = opt.map(_.toString).getOrElse("NO SUCH ROUTE")
  }

  def handleQuery(service: TrainService, q: Query): String= {
    implicit class ShowWalk(nodes: List[service.NodeSeq]) {
      def show: String = nodes.length.toString
    }

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
  }

  def rejectQuery(q: Query): FSM.State[TrainActor.State, Data] = {
    val reject = RejectedQuery(q, s"cannot satisfy query ${q.show} as train network is offline")
    sender() ! reject
    log.warning(reject.toString)
    goto(Offline) using Uninitialized
  }

  def handleNetworkCreateCommand(edgeCount: Int, weightedEdges: List[RawWeightedEdge]): FSM.State[TrainActor.State, Data] = {
    val command = NetworkCreateCommand(edgeCount, weightedEdges)
    TrainService.createRoutes(edgeCount, weightedEdges).map(TrainService(_)) match {
      case None =>
        val reject = RejectedCommand(s"invalid command $command")
        sender() ! reject
        log.warning(reject.toString)
        goto(Offline) using Uninitialized
      case Some(service) =>
        val msg = s"instantiated train network from $command"
        sender() ! AcceptedCommand(msg)
        log.info(msg)
        goto(Active) using TrainGraph(service)
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

  object Messages {

    // Supported Messages: Response, Query and NetworkCreateCommand
    sealed trait Response
    final case class RejectedQuery(q: Query, msg: String)
    final case class AcceptedQuery(q: Query, result: String)
    final case class AcceptedCommand(msg: String)
    final case class RejectedCommand(msg: String)

    sealed trait Query {
      def show: String
    }

    final case class Distance(walk: Seq[Char]) extends Query {
      def show: String = {
        val fmtWalk = new String(walk.toArray)
        s"Distance($fmtWalk)"
      }
    }

    final case class WalksMaxHopsSelectLast(s: Char, t: Char, limit: Int) extends Query {
      def show: String = s"WalksMaxHopsSelectLast($s, $t, $limit)"
    }

    final case class WalksExactSelectLast(s: Char, t: Char, limit: Int) extends Query {
      def show: String = s"WalksExactSelectLast($s, $t, $limit)"
    }

    final case class ShortestRoute(s: Char, t: Char) extends Query {
      def show: String = s"ShortestRoute($s, $t)"
    }

    final case class WalksWithinDistanceSelectLast(s: Char, t: Char, limit: Int) extends Query {
      def show: String = s"WalksWithinDistanceSelectLast($s, $t, $limit)"
    }

    final case class NetworkCreateCommand(edgeCount: Int, weightedEdges: List[RawWeightedEdge])
  }

  import Messages._
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
