package com.deromefintech.trains

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge

object Trains extends App {
  def outputTest(n: Int, result: String): Unit =
    println(s"Output #$n: $result")

  def parseInput(line: String): Option[Graph[Char, WDiEdge]] = {
    val tokens = line
      .replace("Graph: ", "")
      .split(", ")
      .toList
    if (tokens.exists(_.length < 3)) None
    else {
      val Pattern = "([a-zA-Z])([a-zA-Z])(\\d+)".r // to extract AB123 into A-B-123
      // edges will not accept nodes going to themselves, if tokens contain some our line is invalid
      val edges = for {
        tok <- tokens
        m <- Pattern.findFirstMatchIn(tok)
        if m.group(1).head != m.group(2).head
      } yield m.group(1).head ~> m.group(2).head % m.group(3).toInt
      // extract AB123 to A, B, 123 and create a directed edge out of them
      if (edges.nonEmpty && edges.lengthCompare(tokens.length) == 0)
        Some(Graph.from(Nil, edges))
      else None
    }
  }

  def run(service: RouteService): Unit = {
    import service._ // makes all public methods available

    /*
     1. The distance of the route A-B-C.
     2. The distance of the route A-D.
     3. The distance of the route A-D-C.
     4. The distance of the route A-E-B-C-D.
     5. The distance of the route A-E-D.
     */
    var testCase = 0
    List("ABC", "AD", "ADC", "AEBCD", "AED").foreach { s =>
      testCase = testCase + 1
      outputTest(testCase, getDistance(s.toList).show)
    }

    /*
    6. The number of trips starting at C and ending at C with a maximum of 3
    stops.  In the sample data below, there are two such trips: C-D-C (2
    stops). and C-E-B-C (3 stops).
     */
    testCase = testCase + 1
    outputTest(testCase, findWalksMaxHopsSelectLast(s = 'C', t = 'C', limit = 3).length.toString)

    /*
    7. The number of trips starting at A and ending at C with exactly 4 stops.
      In the sample data below, there are three such trips: A to C (via B,C,D); A
    to C (via D,C,D); and A to C (via D,E,B).
     */
    testCase = testCase + 1
    outputTest(testCase, findWalksExactSelectLast(s = 'A', t = 'C', limit = 4).length.toString)

    /*
    8. The length of the shortestRoute route (in terms of distance to travel) from A
    to C.
     */
    testCase = testCase + 1
    outputTest(testCase, shortestRoute(s = 'A', t = 'C').show)

    /*
    9. The length of the shortestRoute route (in terms of distance to travel) from B
    to B.
     */
    testCase = testCase + 1
    outputTest(testCase, shortestRoute(s = 'B', t = 'B').show)

    /*
    10. The number of different routes from C to C with a distance of less than
    30.  In the sample data, the trips are: CDC, CEBC, CEBCDC, CDCEBC, CDEBC,
    CEBCEBC, CEBCEBCEBC. So 7 of them.
     */
    testCase = testCase + 1
    outputTest(testCase, exploreWalksWithinDistanceSelectLast('C', 'C', 30).length.toString)
  }

  println("enter graph for example Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
  val input = scala.io.StdIn.readLine()
  parseInput(input) match {
    case None => println("invalid input")
    case Some(routes) =>
      run(new RouteService(routes))
  }
}
