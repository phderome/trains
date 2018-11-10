package com.deromefintech.trains

import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

import scala.collection.mutable.ListBuffer
import cats.implicits._

import scala.language.postfixOps

final class RouteService(val routes: Graph[Char, WDiEdge]) {

  type NodeSeq = Vector[Char]

  object NodeSeq {
    @inline def apply(c: Char): NodeSeq = Vector(c)
  }

  implicit class ShowOptionalDistance(opt: Option[Int]) {
    def show: String = opt.map(_.toString).getOrElse("NO SUCH ROUTE")
  }

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

  // all we are doing is using the Graph API to construct a valid walk if input is valid and ask API to give us back the weight.
  def getDistance(walk: Seq[Char]): Option[Int] = {
    if (walk.lengthCompare(2) < 0) None
    else {
      val walkBuilder = node(walk.head).map(n => routes.newWalkBuilder(n)(walk.length))
      for {
        wBuilder <- walkBuilder
        dist <- if (walk.drop(1).forall { x =>
          (for {
            n <- node(x)
          } yield wBuilder.add(n)).nonEmpty
        })
          Some(wBuilder.result.weight.toInt)
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

  def exploreWalksWithinDistance(u: Char, limit: Int, predicate: NodeSeq => Boolean = _ => true): Seq[NodeSeq] = {
    // this is BFS algorithm and interestingly the library supports it natively but seems to refuse to walk through cycles
    // for a while as is required here.
    val allWalks = ListBuffer(NodeSeq(u))
    var eligibleWalks: Seq[NodeSeq] = List(NodeSeq(u))
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
    allWalks.filter(combinePredWithNonZeroLength(predicate))
  }

  def findWalksExactSelectLast(s: Char, t: Char, limit: Int): List[NodeSeq] =
    findWalksExact(s, limit, selectLast(t))

  def findWalksMaxHopsSelectLast(s: Char, t: Char, limit: Int): List[NodeSeq] =
    findWalksMaxHops(s, limit, selectLast(t))

  def exploreWalksWithinDistanceSelectLast(s: Char, t: Char, limit: Int): Seq[NodeSeq] =
    exploreWalksWithinDistance(s, limit, selectLast(t))
}
