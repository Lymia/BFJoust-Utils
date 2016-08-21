package moe.lymia.bfjoust.vm

import java.util

sealed trait ArenaStatus
object ArenaStatus {
  case object Running  extends ArenaStatus
  case object LeftWon  extends ArenaStatus
  case object RightWon extends ArenaStatus
  case object Tied     extends ArenaStatus
}

sealed trait ArenaAction
object ArenaAction {
  case object IncData extends ArenaAction
  case object DecData extends ArenaAction
  case object IncPtr  extends ArenaAction
  case object DecPtr  extends ArenaAction
  case object NullOp  extends ArenaAction
}

final class ArenaPlayer private[vm] (var dp: Int, flagPos: Int, polarityModifier: Int, pointerModifier: Int,
                                     private var lost: Boolean = false, private var lastState: Boolean = false) {
  def hasLost = lost

  def executeAction(arena: Arena, action: ArenaAction) = action match {
    case ArenaAction.IncData => arena.tape(dp) = (arena.tape(dp) + polarityModifier).toByte
    case ArenaAction.DecData => arena.tape(dp) = (arena.tape(dp) - polarityModifier).toByte
    case ArenaAction.IncPtr  => dp = dp + pointerModifier
    case ArenaAction.DecPtr  => dp = dp - pointerModifier
    case ArenaAction.NullOp  =>
  }
  def test(arena: Arena) = arena.tape(dp) != 0

  def cycleEnd(arena: Arena) = {
    val isZero = arena.tape(flagPos) == 0
    if(lastState && isZero) lost = true
    lastState = isZero

    if(dp < 0 || dp >= arena.tape.length) lost = true
  }

  def printDp(arena: Arena) =
    println("dp = "+dp+", tape(dp) = "+(if(dp < 0 || dp >= arena.tape.length) "<out of bounds>" else arena.tape(dp)))

  override def clone() = new ArenaPlayer(dp, flagPos, polarityModifier, pointerModifier, lost, lastState)
}
final class Arena private (val tape: Array[Byte], val left: ArenaPlayer, val right: ArenaPlayer,
                           private var arenaStatus: ArenaStatus = ArenaStatus.Running, private var ctick: Int = 0) {
  def tick = ctick
  def status = arenaStatus

  def cycleEnd() {
    if(arenaStatus != ArenaStatus.Running) return

    left .cycleEnd(this)
    right.cycleEnd(this)

    val leftLost  = left .hasLost
    val rightLost = right.hasLost

    ctick = ctick + 1

         if(leftLost && rightLost) arenaStatus = ArenaStatus.Tied
    else if(leftLost             ) arenaStatus = ArenaStatus.RightWon
    else if(rightLost            ) arenaStatus = ArenaStatus.LeftWon
    else if(ctick == 100000      ) arenaStatus = ArenaStatus.Tied
  }

  def printTape() = println("%5d - ".format(ctick)+tape.zipWithIndex.map(x => "%c%02X".format({
    val leftOnTile  = left .dp == x._2
    val rightOnTile = right.dp == x._2
    if(leftOnTile && rightOnTile) 'X' else if(leftOnTile) '>' else if(rightOnTile) '<' else ' '
  }, x._1)).reduce(_ + " " + _))

  override def clone() =
    new Arena(util.Arrays.copyOf(tape, tape.length), left.clone(), right.clone(), arenaStatus, ctick)
}
object Arena {
  def makeTape(length: Int) = {
    val array = new Array[Byte](length)
    array(0) = -128
    array(length - 1) = -128
    array
  }

  def apply(tapeLength: Int, polarity: Boolean) = {
    val left = new ArenaPlayer(0, 0, 1, 1)
    val right = new ArenaPlayer(tapeLength - 1, tapeLength - 1, if(polarity) -1 else 1, -1)
    new Arena(makeTape(tapeLength), left, right)
  }
}

case class Battle(leftCode: Program, rightCode: Program, tapeLength: Int, polarity: Boolean) {
  val left  = BytecodeEvaluator(leftCode)
  val right = BytecodeEvaluator(rightCode)
  val arena = Arena(tapeLength, polarity)

  def status = arena.status
  def cycle(trace: Boolean = false, traceAll: Boolean = false) = {
    val testRight = arena.right.test(arena)
    if(traceAll) {
      print(":: left  ::  "); arena.left.printDp(arena)
      print(":: right ::  "); arena.right.printDp(arena)
    }
    arena.left.executeAction(arena, left.cycle(arena.left.test(arena), traceAll, ":: left  ::  "))
    arena.right.executeAction(arena, right.cycle(testRight, traceAll, ":: right ::  "))
    if(trace) {
      arena.printTape()
      if(traceAll) println()
    }
    arena.cycleEnd()
  }
}