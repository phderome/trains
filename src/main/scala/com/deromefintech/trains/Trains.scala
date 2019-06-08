package com.deromefintech.trains

import com.deromefintech.trains.TrainService.{NetworkRequest, RawWeightedEdge}
import akka.actor._

import scala.concurrent.Await
import scala.concurrent.duration._

object Trains extends App {

  def getNetworkRequest(line: String): NetworkRequest = {
    val tokens = line
      .split(", ")
      .toList
    if (tokens.exists(_.length < 3)) NetworkRequest(0, List.empty[RawWeightedEdge])
    else {
      val Pattern = "([a-zA-Z])([a-zA-Z])(\\d+)".r // to extract AB123 into A-B-123
      // edges will not accept nodes going to themselves, if tokens contain some our line is invalid
      val edges: List[(Char, Char, Int)] = for {
        tok <- tokens
        m <- Pattern.findFirstMatchIn(tok)
        a = m.group(1).head
        b = m.group(2).head
        if a != b
        weight = m.group(3).toInt
      } yield (a, b, weight)
      val wEdges = edges.map { case(s, t, w) => RawWeightedEdge(s, t, w) }
      NetworkRequest(tokens.length, wEdges)
    }
  }

  val system = ActorSystem("Trains")
  val trainActor = system.actorOf(Props[TrainActor], name = "trainactor")

  (1 to 2).foreach { _ =>
    println("enter graph for example Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val line = scala.io.StdIn.readLine()
    trainActor ! getNetworkRequest(line)
    getSampleQueries.foreach(q => trainActor ! q)
  }
  Thread.sleep(7000)
  Await.ready(system.terminate(), 20.seconds)

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
