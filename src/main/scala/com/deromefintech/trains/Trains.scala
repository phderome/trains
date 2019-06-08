package com.deromefintech.trains

import akka.actor._

import scala.concurrent.Await
import scala.concurrent.duration._

object Trains extends App {
  import TrainActor.Messages._

  def getNetworkCreateCommand(line: String): NetworkCreateCommand = {
    val tokens = line
      .split(", ")
      .toList
    if (tokens.exists(_.length < 3)) NetworkCreateCommand(0, List.empty[RawWeightedEdge])
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
      NetworkCreateCommand(tokens.length, wEdges)
    }
  }

  val system = ActorSystem("Trains")
  val trainActor = system.actorOf(Props[TrainActor], TrainActor.Name)

  (1 to 2).foreach { _ =>
    println("enter graph for example Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val line = scala.io.StdIn.readLine()
    trainActor ! getNetworkCreateCommand(line)
    TrainActor.getSampleQueries.foreach(q => trainActor ! q)
  }
  Await.ready(system.terminate(), 5.seconds)
}
