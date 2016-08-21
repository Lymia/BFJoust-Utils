package moe.lymia.bfjoust.solver

import java.io.File

import moe.lymia.bfjoust.vm._

class HillEvaluation(var states: Seq[ProgramState]) {
  var origLength = states.length

  def evaluate() = {
    states.foreach(_.evaluate())
  }
  def tickEnd() = {
    states.foreach(_.tickEnd())
    val result = states.exists(x => x.hasWon || x.hasTied)
    states = states.filter(!_.isEnded)
    result
  }

  def minTape = if(states.isEmpty) 30 else states.minBy(_.tape.length).tape.length
  def remaining = states.count{x => !x.isEnded}
  def won = origLength - remaining

  def histogram(tapePos: Int) =
    states.filter(x => x.tape.length > tapePos && !x.isEnded).foldLeft(Map[Byte, Int]())((m, x) => {
      val b = x.tape(tapePos)
      m.updated(b, m.getOrElse(b, 0) + 1)
    })
  def addTo(dp: Int, amount: Int) = states.foreach(_.addTo(dp, amount))
  def test(dp: Int) = states.map(_.clone()).partition(_.tape(dp) != 0)

  override def clone() = {
    val he = new HillEvaluation(states.map(_.clone()))
    he.origLength = origLength
    he
  }
}
object HillEvaluation {
  def loadProgram(file: File) = {
    System.out.println("Loading: "+file)
    val source = scala.io.Source.fromFile(file).mkString
    val result = Parser(source)
    ProgramState.generateTapes(file.toString, Compiler(result.left.getOrElse(sys.error(result.right.get))))
  }
  def loadProgramDirectory(file: File) = file.listFiles().filter(_.isFile).flatMap(loadProgram)

  def apply(file: File) = new HillEvaluation(loadProgramDirectory(file))
}