package com.deromefintech.trains.domain.model

sealed trait Response
final case class RejectedQuery(q: Query, msg: String)
final case class AcceptedQuery(q: Query, result: String)
final case class RejectedCommand(cmd: Command, msg: String)
