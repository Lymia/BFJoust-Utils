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

import moe.lymia.bfjoust.vm.ArenaAction

class Cursor(var dp: Int, var hill: ParallelEvaluator,
             var winCount: Int = 0, var fragment: StringBuilder = new StringBuilder()) {
  private var last = "XXX"
  private var lastCount = 0
  def append(str: String) = {
    if(str != last) {
      if(lastCount == 0) { }
      else if(lastCount == 1) fragment.append(last)
      else if((last.length * lastCount) > (lastCount.toString.length + 3 + last.length)) {
        fragment.append("(")
        fragment.append(last)
        fragment.append(")")
        fragment.append("*")
        fragment.append(lastCount)
      } else for(i <- 0 until lastCount) fragment.append(last)

      last = str
      lastCount = 1
    } else lastCount = lastCount + 1
  }

  def forward() = {
    hill.cycle(ArenaAction.IncPtr)
    dp = dp + 1
    append(">")
  }
  def backward() = {
    hill.cycle(ArenaAction.DecPtr)
    dp = dp - 1
    append("<")
  }
  def goto(ndp: Int) = {
    val diff  = ndp - dp
    val count = Math.abs(diff)
    for(i <- 0 until count) if(diff < 0) backward() else forward()
  }

  def nop() = {
    hill.cycle(ArenaAction.NullOp)
    append(".")
  }
  def sleep(ticks: Int) =  for(i <- 0 until ticks) nop()

  def inc() = {
    hill.cycle(ArenaAction.IncData)
    append("+")
  }
  def dec() = {
    hill.cycle(ArenaAction.DecData)
    append("-")
  }
  def add(diff: Int) {
    val count = Math.abs(diff)
    for(i <- 0 until count) if(diff < 0) dec() else inc()
  }

  private def dutySection(add: Boolean, duty: Int, count: Int) = {
    val char = if(add) "+" else "-"
    val str = (if(duty > 5) "("+char+")*"+(duty-1) else Array.fill(duty - 1)(char).mkString("")) + "."

    for(i <- 0 until count) {
      for(i <- 0 until duty - 1) if(add) hill.cycle(ArenaAction.IncData) else hill.cycle(ArenaAction.DecData)
      hill.cycle(ArenaAction.NullOp)
      append(str)
    }
  }
  def add(diff: Int, duty: Int) {
    val count = Math.abs(diff)
    if(diff == 0) { }
    else if(duty <= 1) add(diff)
    else {
      dutySection(diff >= 0, duty, count / duty)
      add(math.signum(diff) * (diff % duty))
    }
  }

  private def appendFragment(string: String, epilogue: String = "(.)*-1") = {
    append(string)
    if(!string.endsWith(epilogue)) append(epilogue)
  }
  def fork(evalFn: Cursor => Unit) = {
    val (a, b) = hill.test(dp)
    val wins = hill.won + winCount

    val aWins = {
      val hea = new Cursor(dp, a, wins)
      hea.hill.cycle(ArenaAction.NullOp)
      evalFn(hea)
      append("[")
      appendFragment(hea.toString())
      append("]")
      hea.hill.won
    }
    {
      val heb = new Cursor(dp, b, aWins + wins)
      heb.hill.cycle(ArenaAction.NullOp)
      evalFn(heb)
      appendFragment(heb.toString())
    }
  }

  def nextThreat(searchLength: Int): Int = {
    val nh = hill.clone()
    for(i <- 1 to searchLength) {
      if({nh.cycle(ArenaAction.NullOp); nh.hasLost}) return i
    }
    searchLength
  }

  def remaining = hill.remaining
  def minTape = hill.minTape
  def isValid = dp >= 0 && dp < hill.minTape
  def histogram(tapePos: Int = this.dp): Map[Byte, Int] = hill.histogram(tapePos)
  def won = winCount + hill.won

  override def clone() = {
    val c = new Cursor(dp, hill.clone(), winCount, fragment.clone())
    c.last = last
    c.lastCount = lastCount
    c
  }
  def become(c: Cursor) = {
    dp = c.dp
    hill = c.hill.clone()
    winCount = c.winCount
    fragment = c.fragment.clone()
    last = c.last
    lastCount = c.lastCount
  }

  override def toString() = {
    append("")
    fragment.toString()
  }
}
