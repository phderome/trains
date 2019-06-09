package com.deromefintech.trains.domain.model

import com.deromefintech.trains.RawWeightedEdge

sealed trait Command
final case class NetworkCreate(edgeCount: Int, weightedEdges: List[RawWeightedEdge]) extends Command
