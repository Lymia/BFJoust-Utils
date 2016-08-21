package moe.lymia.bfjoust.solver

object Strategy {
  def setInitialDecoys(c: Cursor) = {
    for(i <- 0 until 8) {
      c.forward()
      if(i % 3 != 2) c.add(if(i % 2 == 0) i else -i)
    }
  }

  def evalPosition(c: Cursor, dp: Int) = {
    val hist = c.histogram(dp)
    (hist.getOrElse(0, 0), c.length)
  }
  def distinguish(c: Cursor, cont: Cursor => Unit, max: Int = 100, actionOpenTicks: Int = 0,
                  doAttack: Boolean = true) = {
    val diff = math.max(math.abs(c.dp - (c.minTape - 1)), c.dp)
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
      val err = c.goto(target) | c.test(nc => {
          println("Distinguishing: "+nc.remaining+"/"+c.remaining)
          cont(nc)
      })
      c.hill.states = Seq()
      err
    } else false
  }

  private val act = Seq[Cursor => Boolean](_.inc(), _.dec(), _.nop())
  def totalDistance(c: Cursor) = {
    val hist = c.histogram()
    hist.map(x => math.abs(x._1) * x._2).sum
  }
  def attack(c: Cursor, openTicks: Int): Boolean = {
    println("Attack cycle - target "+(c.minTape - 1)+", open ticks "+openTicks)
    if(openTicks > 0) {
      if(c.goto(c.minTape - 1)) return true

      var i = openTicks
      while(i > 1) {
        val initRem = c.remaining

        val (next, ct) = (for(t0 <- act; t1 <- act) yield {
          val next = c.clone()
          if(t0(next) | t1(next)) Seq()
          else Seq((next, initRem - next.remaining))
        }).flatten.maxBy(_._2)

        if(ct == 0) {
          val maxNudge = math.min(openTicks - 3, 10)
          val data = for(d <- 0 to 8; i <- -maxNudge to maxNudge) yield {
            val next = c.clone()
            val abs  = math.abs(i)
            if(if(i == 0) next.nop() else next.add(i, d)) (next, 10000, -10000)
            else (next, totalDistance(next), i)
          }
          val (nc, best, adj) = data.minBy(_._2)
          i = i - math.abs(adj)
          println("Best attack adjustment: "+best+", adjustment "+adj)
          c.become(next)
        } else {
          val success = next.nop()
          println("####### Defeated "+ct)
          c.become(next)
          return success
        }

        i = i - 1
      }
    }
    false
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
        ic.become(c)
        return
      }

      if(c.goto(0)) sys.error("could not defend")
      val maxNudge = math.max(nextThreat - 1, 5)
      val (nc, best, adj) = (for(d <- 0 to 8;
                                 i <- -maxNudge to maxNudge) yield {
        val next = c.clone()
        val abs  = math.abs(i)
        if(if(i == 0) next.nop() else next.add(i, d)) (next, -1, -10000)
        else {
          (next, next.nextThreat(searchDistance), i)
        }
      }).maxBy(_._2)
      if(best == -1 || adj == -10000) sys.error("could not defend")

      println("New threat distance from best: "+best+", adjustment "+adj)

      c = nc
    }
  }

  def mainLoop(c: Cursor) {
    var phase = false
    while(c.remaining > 0) {
      val nextThreat      = c.nextThreat(100) - 5
      println("Main loop - next threat: "+nextThreat+", remaining: "+c.remaining)
      val maxDistinguish  = (nextThreat - 1) / 2
      val actionOpenTicks = math.max(nextThreat - c.minTape * 2, 0)
      if(nextThreat > 20) {
        if(if(!phase) distinguish(c, mainLoop, max = maxDistinguish, actionOpenTicks = actionOpenTicks) else
                      attack(c, actionOpenTicks)) sys.error("died while acting")
      }
      phase = !phase
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
