package com.deromefintech.trains.domain.model

import com.deromefintech.trains.RawWeightedEdge

sealed trait Event
final case class NetworkCreated(edgeCount: Int, weightedEdges: List[RawWeightedEdge]) extends Event

final case class EdgeDeleted(edge: RawWeightedEdge) extends Event

final case class EdgeUpdated(edge: RawWeightedEdge) extends Event
