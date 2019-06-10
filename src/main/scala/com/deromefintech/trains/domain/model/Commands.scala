package com.deromefintech.trains.domain.model

sealed trait Command
final case class NetworkCreate(edgeCount: Int, weightedEdges: List[RawWeightedEdge]) extends Command

final case class DeleteEdge(edge: RawWeightedEdge) extends Command

final case class UpdateEdge(edge: RawWeightedEdge, formerWeight: Int) extends Command

