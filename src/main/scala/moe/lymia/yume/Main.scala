package moe.lymia.yume

import java.io.PrintWriter

object Main {
  def main(args: Array[String]) = {
    val c = new Cursor(0, HillEvaluation(new java.io.File("hill")))
    Strategy.executeStrategy(c)

    val pw = new PrintWriter("out.bfjoust")
    pw.println(c.toString())
    pw.close()
  }
}
