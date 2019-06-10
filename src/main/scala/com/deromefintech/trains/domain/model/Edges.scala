package com.deromefintech.trains.domain.model

final case class RawEdge(s: Char, t: Char)
final case class RawWeightedEdge(edge: RawEdge, w: Int)
