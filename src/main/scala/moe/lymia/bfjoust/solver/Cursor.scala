package moe.lymia.bfjoust.solver

class Cursor(var dp: Int, var hill: HillEvaluation, var fragment: StringBuilder = new StringBuilder()) {
  private var last = "XXX"
  private var lastCount = 0
  def append(str: String) = {
    if(str != last) {
      if(lastCount == 0) { }
      else if(lastCount == 1) fragment.append(last)
      else if(last.length * lastCount > lastCount.toString.length + 3 + last.length) {
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
    hill.evaluate()
    dp = dp + 1
    append(">")
    hill.tickEnd()
  }
  def backward() = {
    hill.evaluate()
    dp = dp - 1
    append("<")
    hill.tickEnd()
  }
  def goto(ndp: Int) = {
    val diff  = ndp - dp
    val count = Math.abs(diff)

    var mistake = false
    for(i <- 0 until count) mistake = mistake | (if(diff < 0) backward() else forward())
    mistake
  }

  def nop() = {
    hill.evaluate()
    append(".")
    hill.tickEnd()
  }
  def sleep(ticks: Int) = {
    var mistake = false
    for(i <- 0 until ticks) mistake = mistake | nop()
    mistake
  }

  def inc() = {
    hill.evaluate()
    hill.addTo(dp, 1)
    append("+")
    hill.tickEnd()
  }
  def dec() = {
    hill.evaluate()
    hill.addTo(dp, -1)
    append("-")
    hill.tickEnd()
  }
  def add(diff: Int) = {
    val count = Math.abs(diff)

    var mistake = false
    for(i <- 0 until count) mistake = mistake | (if(diff < 0) dec() else inc())

    mistake
  }
  def add(diff: Int, duty: Int) = {
    val count = Math.abs(diff)

    var mistake = false
    for(i <- 0 until count) mistake = mistake | (if(duty == 0 || i % duty != (duty - 1))
                                                    if(diff < 0) dec() else inc() else nop())

    mistake
  }

  def test(evalFn: Cursor => Unit) = {
    val (a, b) = hill.test(dp)
    val (hea, heb) = (new Cursor(dp, new HillEvaluation(a)), new Cursor(dp, new HillEvaluation(b)))

    hea.hill.evaluate()
    heb.hill.evaluate()
    val mistake = hea.hill.tickEnd() | heb.hill.tickEnd()
    if(!mistake) {
      evalFn(hea)
      evalFn(heb)

      append("[")
      append(hea.toString())
      append("]")
      append(heb.toString())
    }

    mistake
  }

  def nextThreat(searchLength: Int): Int = {
    val nh = hill.clone()
    for(i <- 1 to searchLength) {
      if({nh.evaluate(); nh.tickEnd()}) return i
    }
    searchLength
  }

  def remaining = hill.remaining
  def length = hill.states.count(!_.isEnded)
  def minTape = hill.minTape
  def isValid = dp >= 0 && dp < hill.minTape
  def histogram(tapePos: Int = this.dp): Map[Byte, Int] = hill.histogram(tapePos)

  override def clone() = {
    val c = new Cursor(dp, hill.clone(), fragment.clone())
    c.last = last
    c.lastCount = lastCount
    c
  }
  def become(c: Cursor) = {
    dp = c.dp
    hill = c.hill.clone()
    fragment = c.fragment.clone()
    last = c.last
    lastCount = c.lastCount
  }

  override def toString() = {
    append("")
    fragment.toString()
  }
}
