package moe.lymia.bfjoust.solver

import moe.lymia.bfjoust.vm._

final case class EnemyState(arena: Arena, right: BytecodeEvaluator) {
  def tape   = arena.tape
  def status = arena.status

  def cycle(action: ArenaAction) = {
    arena.right.executeAction(arena, right.cycle(arena.right.test(arena)))
    arena.left.executeAction(arena, action)
    arena.cycleEnd()
  }

  override def clone() = EnemyState(arena.clone(), right.clone())
}

final class ParallelEvaluator private (var states: Seq[EnemyState], val origCount: Int,
                                       val totalCount: Int, private var lost: Boolean,
                                       var ctick: Int) {
  def cycle(action: ArenaAction) = {
    states.foreach(_.cycle(action))
    if(states.exists(x => x.status == ArenaStatus.RightWon || x.status == ArenaStatus.Tied)) lost = true
    states = states.filter(_.status == ArenaStatus.Running)
    ctick = ctick + 1
  }

  def minTape   = if(states.isEmpty) 30 else states.minBy(_.tape.length).tape.length
  def remaining = states.length
  def won       = origCount - remaining
  def hasLost   = lost
  def tick      = ctick

  def histogram(tapePos: Int) =
    states.filter(x => x.tape.length > tapePos).foldLeft(Map[Byte, Int]())((m, x) => {
      val b = x.tape(tapePos)
      m.updated(b, m.getOrElse(b, 0) + 1)
    })
  def test(dp: Int) = {
    val (a, b) = states.map(_.clone()).partition(_.tape(dp) != 0)
    (new ParallelEvaluator(a, a.length, totalCount, lost, ctick),
     new ParallelEvaluator(b, b.length, totalCount, lost, ctick))
  }

  override def clone() = new ParallelEvaluator(states.map(_.clone()), origCount, totalCount, lost, ctick)
}
object ParallelEvaluator {
  def makeEnemyStates(programs: Seq[Program]) = programs.flatMap(program =>
    for(tapeLength <- 10 to 30; polarity <- Seq(true, false))
      yield EnemyState(Arena(tapeLength, polarity), BytecodeEvaluator(program)))
  def apply(programs: Seq[Program]) = {
    val states = makeEnemyStates(programs)
    new ParallelEvaluator(states, states.length, states.length, false, 0)
  }
}