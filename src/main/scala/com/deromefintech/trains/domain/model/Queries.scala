package com.deromefintech.trains.domain.model

sealed trait Query {
  def show: String
}

final case class Distance(walk: String) extends Query {
  def show: String = s"Distance($walk)"
}

final case class WalksMaxHopsSelectLast(s: Char, t: Char, limit: Int) extends Query {
  def show: String = s"WalksMaxHopsSelectLast($s, $t, $limit)"
}

final case class WalksExactSelectLast(s: Char, t: Char, limit: Int) extends Query {
  def show: String = s"WalksExactSelectLast($s, $t, $limit)"
}

final case class ShortestRoute(s: Char, t: Char) extends Query {
  def show: String = s"ShortestRoute($s, $t)"
}

final case class WalksWithinDistanceSelectLast(s: Char, t: Char, limit: Int) extends Query {
  def show: String = s"WalksWithinDistanceSelectLast($s, $t, $limit)"
}
