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

object Solver {
  val duties = 1 to 12

  def setInitialDecoys(c: Cursor) = {
    for(i <- 0 until 8) {
      c.forward()
      if(i % 3 != 2) c.add(if(i % 2 == 0) i else -i)
    }
  }

  def evalPosition(c: Cursor, dp: Int) = {
    val hist = c.histogram(dp)
    (hist.getOrElse(0, 0), c.remaining)
  }
  def distinguish(c: Cursor, cont: Cursor => Unit, openTicks: Int) = {
    val diff = math.min(math.max(math.abs(c.dp - (c.minTape - 1)), c.dp), (openTicks - 1) / 2)
    val tmp = c.clone()
    val (target, ratio, success) = (for(i <- 0 to diff) yield {
      def position(i: Int) = if(i >= 0 && i < c.minTape) {
        val (count, total) = evalPosition(c, i)
        Seq((i, math.abs((count.toDouble / total.toDouble) - 0.5), count > 0 && (total - count) > 0))
      } else Seq()
      val result = Seq(position(c.dp + i), position(c.dp - i)).flatten
      tmp.nop()
      result
    }).flatten.minBy(_._2)

    println("Distinguishing - best position: "+target+", best ratio: "+ratio)
    if(success) {
      c.goto(target)
      c.fork(nc => {
          println("Current fork: "+nc.remaining+"/"+c.remaining)
          cont(nc)
      })

      c.hill.states = Seq()
      true
    } else false
  }

  private val act = Seq[Cursor => Unit](_.inc(), _.dec(), _.nop())
  def evaluateDistances(c: Cursor) = {
    val hist = c.histogram()
    hist.map(x => math.log(129 - math.abs(x._1))).sum
  }
  def closest(c: Cursor) = {
    val hist = c.histogram()
    if(hist.isEmpty) -1 else math.abs(hist.minBy(x => math.abs(x._1))._1)
  }
  def attack(c: Cursor, rawOpenTicks: Int) {
    val openTicks = math.max(rawOpenTicks - c.minTape * 2, 0)
    println("Attack cycle - target "+(c.minTape - 1)+", open ticks "+openTicks)
    if(openTicks > 0) {
      c.goto(c.minTape - 1)

      var i = openTicks
      while(i > 2) {
        val initWon = c.won

        val data = (for(t0 <- act; t1 <- act; t2 <- act) yield {
          val next = c.clone()
          for(op <- Seq(t0, t1, t2)) op(next)
          if(next.hill.hasLost) Seq()
          else Seq((next, next.won - initWon))
        }).flatten
        val (next, ct) = if(data.isEmpty) (null, 0) else data.maxBy(_._2)

        if(ct == 0) {
          val maxNudge = i - 5
          if(maxNudge <= 0) return
          val data = (for(d <- duties; i <- -maxNudge to maxNudge) yield {
            val next = c.clone()
            if(i == 0) next.nop() else next.add(i, d)
            if(next.hill.hasLost) Seq()
            else {
              val target = next.clone()
              for(i <- 0 until 5) target.nop()
              Seq((next, (next.won, -closest(target), evaluateDistances(target)), i))
            }
          }).flatten
          if(data.isEmpty) return
          val (next, best, adj) = data.maxBy(_._2)
          i = i - (next.hill.tick - c.hill.tick)
          println("Best attack adjustment: "+best+", adjustment "+adj)
          c.become(next)
        } else c.become(next)

        if(initWon != c.won) {
          println("####### Defeated "+c.won+"/"+c.hill.totalCount)
          return
        }

        i = i - 1
      }
    }
  }

  def defend(ic: Cursor) {
    println("Defend cycle.")

    var c = ic
    val searchDistance = 100
    val target = math.max(40, c.minTape * 2 + 10)
    while(true) {
      val nextThreat = c.nextThreat(searchDistance)
      println("Defend cycle - next threat: "+nextThreat+", target: "+target)
      if(nextThreat > target) {
        if(ic.hill.hasLost) sys.error("failed to defend")
        ic.become(c)
        return
      }

      c.goto(0)
      val maxNudge = math.max(nextThreat - 1, 5)
      val (nc, best, adj) = (for(d <- duties; i <- -maxNudge to maxNudge) yield {
        val next = c.clone()
        if(i == 0) next.nop() else next.add(i, d)
        if(next.hill.hasLost) (next, -1, -10000)
        else                  (next, next.nextThreat(searchDistance), i)
      }).maxBy(_._2)
      if(best == -1 || adj == -10000) sys.error("could not defend")

      println("New threat distance from best: "+best+", adjustment "+adj)

      c = nc
    }
  }

  def mainLoop(c: Cursor) {
    while(c.remaining > 0) {
      val openTicks       = c.nextThreat(100) - 5 // keep 5 ticks open for defending
      println("Main loop - open ticks: "+openTicks+", remaining: "+c.remaining)
      if(!distinguish(c, mainLoop, openTicks)) attack(c, openTicks)
      if(c.remaining > 0) defend(c)
    }
    println("Finished fragment.")
    c.append("")
  }

  def executeStrategy(c: Cursor) = {
    setInitialDecoys(c)
    mainLoop(c)
    println("Result: "+c.fragment.toString())
  }
}
