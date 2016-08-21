/*
 * Copyright (c) 2016 Lymia Alusyia <lymia@lymiahugs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

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
                                       val totalCount: Int, private var lost: Int,
                                       var ctick: Int) {
  def cycle(action: ArenaAction) = {
    states.foreach(_.cycle(action))
    lost = lost + states.count(x => x.status == ArenaStatus.RightWon || x.status == ArenaStatus.Tied)
    states = states.filter(_.status == ArenaStatus.Running)
    ctick = ctick + 1
  }

  def minTape   = if(states.isEmpty) 30 else states.minBy(_.tape.length).tape.length
  def remaining = states.length
  def won       = origCount - remaining - lost
  def hasLost   = lost > 0
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
    new ParallelEvaluator(states, states.length, states.length, 0, 0)
  }
}