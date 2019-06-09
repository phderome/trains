package com.deromefintech.trains

import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

import cats.implicits._

import scala.collection.mutable.ListBuffer

final case class TrainService(routes: Graph[Char, WDiEdge])  {
  import TrainService._

  implicit class RichOptionNodeT(mNode0: Option[routes.NodeT]) {
    def shortestPathTo(mNode1: Option[routes.NodeT]): Option[routes.Path] =
      (mNode0, mNode1).mapN(_ shortestPathTo _).flatten

    def diSuccessors: Set[routes.NodeT] =
      mNode0.fold(Set.empty: Set[routes.NodeT])(_.diSuccessors)

    def findOutgoingTo(mNode1: Option[routes.NodeT]): Option[routes.EdgeT] =
      (mNode0, mNode1).mapN(_ findOutgoingTo _).flatten

    def getOneHopDistance(mNode1: Option[routes.NodeT]): Option[Int] =
      findOutgoingTo(mNode1).map(_.weight.toInt)

    def shortestDistinct(mNode1: Option[routes.NodeT]): Option[Int] =
      shortestPathTo(mNode1).map(_.weight.toInt)

    def shortestSame: Option[Int] = {
      // requirements do not accept a value of 0 here and insist on doing a non-trivial cycle
      val lengths =
        for {
          next <- diSuccessors.map(node(_))
          length <- (next.shortestDistinct(mNode0), getOneHopDistance(next)).mapN(_ + _)
        } yield length
      if (lengths.isEmpty) None else Some(lengths.min)
    }
  }

  def node(outer: Char): Option[routes.NodeT] = routes find outer

  val selectLast: Char => NodeSeq => Boolean = t => ns => ns.last == t

  def shortestDistinct(s: Char, t: Char): Option[Int] =
    node(s).shortestDistinct(node(t))

  def shortestSame(s: Char): Option[Int] =
    node(s).shortestSame

  def shortestRoute(s: Char, t: Char): Option[Int] =
    if (s == t) shortestSame(s)
    else shortestDistinct(s, t)

  def getOneHopDistance(s: Char, t: Char): Option[Int] =
    node(s).getOneHopDistance(node(t))

  // all we are doing is using the Graph API to construct a valid walk if walk is valid and ask API to give us back the weight.
  def getDistance(walk: Seq[Char]): Option[Int] = {
    if (walk.lengthCompare(2) < 0) None
    else {
      val walkBuilder = node(walk.head).map(h => routes.newWalkBuilder(h)(walk.length))
      for {
        wBuilder <- walkBuilder
        dist <- if (walk.drop(1).forall { x =>
          (for {
            n <- node(x)
          } yield wBuilder.add(n)).nonEmpty
        }) {
          val walkFromNext = wBuilder.result
          if (walkFromNext.length + 1 == walk.length) Some(walkFromNext.weight.toInt) else None
        }
        else None
      } yield dist
    }
  }

  def findWalksExact(u: Char, limit: Int, p: NodeSeq => Boolean = _ => true): List[NodeSeq] = {
    def explore: List[NodeSeq] = {
      (1 to limit).foldLeft(List(NodeSeq(u))) {
        case (eligibleWalks, _) =>
          for {
            walk <- eligibleWalks
            successor <- node(walk.last).diSuccessors.toList
            newWalk = walk :+ successor.toOuter
          } yield newWalk
      }
    }

    if (limit == 0) Nil // need to suppress walks of length 0
    else explore.filter(p)
  }

  val combinePredWithNonZeroLength: (NodeSeq => Boolean) => NodeSeq => Boolean =
    predicate => path => path.lengthCompare(1) > 0 && predicate(path)

  def findWalksMaxHops(u: Char, limit: Int, p: NodeSeq => Boolean = _ => true): List[NodeSeq] = {
    val (totalWalks, _) =
      (1 to limit).foldLeft((List(NodeSeq(u)), List(NodeSeq(u)))) {
        case ((walks, currGenWalks), _) =>
          val nextGenWalks =
            for {
              walk <- currGenWalks
              next <- node(walk.last).diSuccessors
            } yield walk :+ next.toOuter
          (walks ++ nextGenWalks, nextGenWalks)
      }
    totalWalks.filter(combinePredWithNonZeroLength(p))
  }

  def exploreWalksWithinDistance(u: Char, limit: Int, predicate: NodeSeq => Boolean = _ => true): List[NodeSeq] = {
    // this is BFS algorithm and interestingly the library supports it natively but seems to refuse to walk through cycles
    // for a while as is required here.
    val allWalks = ListBuffer(NodeSeq(u))
    var eligibleWalks: List[NodeSeq] = List(NodeSeq(u))
    while (eligibleWalks.nonEmpty) {
      val nextEligibleWalks =
        for {
          walk <- eligibleWalks
          successor <- node(walk.last).diSuccessors.toList
          newWalk = walk :+ successor.toOuter
          newWalkDistance <- getDistance(newWalk) if newWalkDistance < limit
        } yield newWalk

      eligibleWalks = nextEligibleWalks
      allWalks.appendAll(eligibleWalks)
    }
    allWalks.filter(combinePredWithNonZeroLength(predicate)).toList
  }

  def findWalksExactSelectLast(s: Char, t: Char, limit: Int): List[NodeSeq] =
    findWalksExact(s, limit, selectLast(t))

  def findWalksMaxHopsSelectLast(s: Char, t: Char, limit: Int): List[NodeSeq] =
    findWalksMaxHops(s, limit, selectLast(t))

  def exploreWalksWithinDistanceSelectLast(s: Char, t: Char, limit: Int): List[NodeSeq] =
    exploreWalksWithinDistance(s, limit, selectLast(t))

  def updateEdge(weightedEdge: RawWeightedEdge, old: Int): Option[TrainService] = {
    for {
      _ <- routes.find(weightedEdge.edge.s)
      _ <- routes.find(weightedEdge.edge.t)
    } yield {
      val newRoutes = (routes - weightedEdge.copy(w = old)) + weightedEdge
      TrainService(newRoutes)
    }
  }

  def deleteEdge(weightedEdge: RawWeightedEdge): Option[TrainService] = {
    routes
      .find(weightedEdge)
      .map { toDelete => TrainService(routes - toDelete) }
  }
}

final case class RawEdge(s: Char, t: Char)
final case class RawWeightedEdge(edge: RawEdge, w: Int)
object RawWeightedEdge {
  implicit def toWDiEdge(e: RawWeightedEdge): WDiEdge[Char] =
    e.edge.s ~> e.edge.t % e.w
}

object TrainService {
  type NodeSeq = Vector[Char]

  object NodeSeq {
    @inline def apply(c: Char): NodeSeq = Vector(c)
  }

  implicit class ShowOptionalDistance(opt: Option[Int]) {
    def show: String = opt.map(_.toString).getOrElse("NO SUCH ROUTE")
  }
  implicit class ShowWalk(nodes: List[TrainService.NodeSeq]) {
    def show: String = nodes.length.toString
  }

  def createRoutes(edgeCount: Int, weightedEdges: List[RawWeightedEdge]): Option[Graph[Char, WDiEdge]] = {
    val edges: List[WDiEdge[Char]] = weightedEdges.map { case RawWeightedEdge(RawEdge(a, b), w) => a ~> b % w }

    // extract AB123 to A, B, 123 and create a directed edge out of them
      if (edges.nonEmpty && edges.lengthCompare(edgeCount) == 0)
        Some(Graph.from(Nil, edges))
      else None
  }

}