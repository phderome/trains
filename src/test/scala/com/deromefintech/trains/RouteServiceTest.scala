package com.deromefintech.trains

import org.scalacheck.Gen
import org.scalacheck.Gen.choose
import org.specs2.ScalaCheck
import org.scalacheck.Prop.forAll
import org.specs2.mutable.Specification
import org.specs2.scalacheck.Parameters
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge

object routeservicespec extends Specification with ScalaCheck {

  // IMPORTANT:
  // Some restrictions on graph construction in tests are intentional with some basic understanding of graph isomorphism assumed.
  // For instance we choose alphabet range A to E as nodes and edges constructed from nodes in A-E range and feel that we can still capture
  //  much of the complexity of the problem we need to validate. Similarly we start our routes inquiries at a head node,
  // which technically precludes empty graphs but indeed there is no service to deliver if there are no stations in the network!
  // Plus, if the input line were empty, with no edges, it is undefined as to which nodes we should add, so I conclude it's invalid.
  // Shut down the business.
  // The restrictions serve a purpose to make test design simpler without sacrificing completeness.

  implicit val params = Parameters(minTestsOk = 200)
  // Scalacheck usage specific note: varying the data in generators too much
  // make ScalaCheck think it does not have enough valid tests and gives up
  // so we keep graphs small with few distinct nodes and few varying weights

  // Trains routes represent a special type of graph: the starting and ending town will not be the same town.
  // So cycles of length 1 (1 edge and 1 node) do not exist. Trains app control
  def noSelfNodeEdgeGen: Gen[WDiEdge[Char]] =
    for {
      s <- choose(65.toChar, 69.toChar)
      t <- choose(65.toChar, 69.toChar)
      if s != t // this is why we have a guard, to meet Trains graph structure
      num <- Gen.choose(1, 2)
    } yield s ~> t % num

  val genEdgeList =
    Gen.listOfN(6, noSelfNodeEdgeGen)

  def uniformEdgeGen: Gen[WDiEdge[Char]] =
    for {
      s <- choose(65.toChar, 69.toChar)
      t <- choose(65.toChar, 69.toChar) if s != t
    } yield s ~> t % 1

  val genUniformEdgeList =
    Gen.listOfN(6, uniformEdgeGen)

  "trainservice must" >> {
    "Number of findWalksMaxHops is equal to the sum of findWalksExact for all values at or below its limit" ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      val root = graph.nodes.head
      val stopsLimit = 6
      val service = new TrainService(graph)
      val allPathsTally = service
        .findWalksMaxHops(root, stopsLimit)
        .length
      val exactTally = (1 to stopsLimit)
        .map(k =>
          service.findWalksExact(root, k).length)
        .sum
      allPathsTally == exactTally
    }

    "findWalksMaxHops do not stop more than promised" ! forAll(genEdgeList) {
      edges =>
        val graph = Graph.from(Nil, edges)
        val stopsLimit = 2
        val service = new TrainService(graph)
        val walkStops =
          service.findWalksMaxHops(graph.nodes.head, stopsLimit).map(_.length - 1)
        walkStops.forall { stops =>
          stops <= stopsLimit
        }
    }

    "findWalksExact do not stop more or less than promised" ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      val stopsLimit = 2
      val service = new TrainService(graph)
      val walkStops =
        service.findWalksExact(graph.nodes.head, stopsLimit).map(_.length - 1)
      walkStops.forall { stops =>
        stops == stopsLimit
      }
    }

    "findWalksExact computes a walk in the graph that library recognizes as such" ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      val stopsLimit = 2
      val service = new TrainService(graph)
      val walks = service.findWalksExact(graph.nodes.head, stopsLimit)
      walks.forall { walk =>
        val nodePairs = walk zip walk.tail
        nodePairs.forall {
          case (s, t) =>
            val eval = for {
              sNode <- service.node(s)
              tNode <- service.node(t)
            } yield sNode.diSuccessors.contains(tNode)
            eval.contains(true)
        }
      }
    }

    "exploreWalksWithinDistance does not retain paths above the allowed limit" ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      val limitDistance = 2
      val service = new TrainService(graph)
      val walkDistances = service
        .exploreWalksWithinDistance(graph.nodes.head, limitDistance)
        .flatMap(service.getDistance)
      walkDistances.forall {
        _ <= limitDistance
      }
    }

    // a walk that consists of weight x should always be eligible for a limit of its own amount of x + 1 (since comparison is strict)
    "exploreWalksWithinDistance allows a single weight to be within its limit" ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      graph.edges.forall { e =>
        val service = new TrainService(graph)
        val nodeSequences: Seq[service.NodeSeq] =
          service.exploreWalksWithinDistance(e.from.toOuter,
            e.weight.toInt + 1)
        nodeSequences.exists { ns =>
          (ns zip ns.tail).exists {
            case (s, t) => s == e.from.toOuter && t == e.to.toOuter
          }
        }
      }
    }

    "getDistance when walk is cyclic, all nodes on cycle have same or less distance to themselves" ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      graph.findCycle.forall { c =>
        val charNodes = c.nodes.map(_.toOuter).toList
        val service = new TrainService(graph)
        val bruteForceDistance = c.edges.map(_.weight.toInt).sum
        charNodes.forall { node =>
          service.shortestSame(node).forall {
            _ <= bruteForceDistance
          }
        }
      }
    }

    "getDistance evaluates the same using brute force" ! forAll(genEdgeList) {
      edges =>
        val graph = Graph.from(Nil, edges)
        graph.findCycle.forall { c =>
          val service = new TrainService(graph)
          val charNodes = c.nodes.map(_.toOuter).toList
          val bruteForceDistance = c.edges.map(_.weight.toInt).sum
          service.getDistance(charNodes) must beSome(bruteForceDistance)
        }
    }

    "getDistance evaluates to number of edges in a graph of weight 1" ! forAll(
      genUniformEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      graph.findCycle.forall { c =>
        val service = new TrainService(graph)
        val charNodes = c.nodes.map(_.toOuter).toList
        service.getDistance(charNodes) must beSome(c.edges.size)
      }
    }

    ("shortestSame is defined iff start node is in a cycle "
      + "that includes itself outgoing and incoming (as opposed to on a path entering a cycle)") ! forAll(
      genEdgeList) { edges =>
      val graph = Graph.from(Nil, edges)
      val root = graph.nodes.head

      val service = new TrainService(graph)
      if (service.shortestSame(root).isDefined)
        root.findCycle.isDefined
      // findCycle may choose a cycle that does not end with h. We could learn the API better to identify
      // a cycle that is not simply reachable from h but also contains it.
      else
        root.findCycle.forall(cycle => cycle.endNode != root) // it may find a cycle further down but will not end here at h

    }

  }
}
