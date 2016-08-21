package moe.lymia.bfjoust.vm

sealed trait ArenaState
object ArenaState {
  case object Running  extends ArenaState
  case object LeftWon  extends ArenaState
  case object RightWon extends ArenaState
  case object Tied     extends ArenaState
}

sealed trait ArenaAction
object ArenaAction {
  case object IncData extends ArenaAction
  case object DecData extends ArenaAction
  case object IncPtr  extends ArenaAction
  case object DecPtr  extends ArenaAction
  case object NullOp  extends ArenaAction
}