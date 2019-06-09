package com.deromefintech.trains

import akka.actor._
import com.deromefintech.trains.domain.model.NetworkCreate

import scala.concurrent.Await
import scala.concurrent.duration._

object Trains extends App {

  def getNetworkCreateCommand(line: String): NetworkCreate = {
    val tokens = line
      .split(", ")
      .toList
    if (tokens.exists(_.length < 3)) NetworkCreate(0, List.empty[RawWeightedEdge])
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
      NetworkCreate(tokens.length, wEdges)
    }
  }

  val system = ActorSystem("Trains")
  val trainActor = system.actorOf(Props[TrainActor], TrainActor.Name)

  (1 to 2).foreach { _ =>
    println("enter graph for example Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val line = scala.io.StdIn.readLine()
    trainActor ! getNetworkCreateCommand(line)
    Thread.sleep(3000) // so that the (new effective) network is available for queries to function as we don't consume feedback messages
    TrainActor.getSampleQueries.foreach(q => trainActor ! q)
  }
  Thread.sleep(1000) // so that we avoid dead letters and can see results back in Akka logging
  Await.ready(system.terminate(), 15.seconds)
}
