package com.deromefintech.trains

import akka.actor.{ActorSystem, Props}
import domain.model.{DeleteEdge, NetworkCreate, UpdateEdge}

import scala.concurrent.Await
import scala.concurrent.duration._

object TrainApp extends App {

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
      val wEdges = edges.map { case(s, t, w) => RawWeightedEdge(RawEdge(s, t), w) }
      NetworkCreate(tokens.length, wEdges)
    }
  }

  val system = ActorSystem("Trains")
  val trainActor = system.actorOf(Props[TrainActor], TrainActor.Name)

  Thread.sleep(2000) // let persistence recover
  TrainActor.getSampleQueries.foreach(q => trainActor ! q)
  // try querying immediately even though on this launch we didn't create a graph
  // On first launch ever, this will get failed queries but on following ones
  // queries will typically succeed.

  (1 to 2).foreach { step =>
    println("enter graph for example Graph: AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7")
    val line = scala.io.StdIn.readLine()
    trainActor ! getNetworkCreateCommand(line)
    if (step == 2) {
      // testing graph editing
      trainActor ! UpdateEdge(RawWeightedEdge(RawEdge('A', 'D'), 9), formerWeight=5)
      trainActor ! DeleteEdge(RawWeightedEdge(RawEdge('E', 'B'), 3)) // success, triggering no route for AEBCD
      trainActor ! DeleteEdge(RawWeightedEdge(RawEdge('A', 'F'), 1)) // failure
    }
    Thread.sleep(3000) // so that the (new effective) network is available for queries to function as we don't consume feedback messages
    TrainActor.getSampleQueries.foreach(q => trainActor ! q)
  }
  Thread.sleep(1000) // so that we avoid dead letters and can see results back in Akka logging
  Await.ready(system.terminate(), 15.seconds)
}
