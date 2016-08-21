package moe.lymia.yume.bfjoust

import java.util

case class ProgramState(name: String, program: Program, polarity: Int, tape: Array[Byte]) {
  import ProgramState.debugPrint

  private var lost = false
  private var won  = false
  private var dead = false

  private var tick = 0

  private var dp = tape.length - 1

  private var ip = 0
  private var rc = new Array[Int](program.idCount)

  private var lastPlayer  = false
  private var lastProgram = false

  def tickEnd() {
    if(won || lost || dead) return

    val playerZero  = tape(0) == 0
    val programZero = tape(tape.length - 1) == 0

    if(lastPlayer && playerZero) won = true
    if((lastProgram && programZero) || (dp < 0) || (dp >= tape.length)) lost = true

    if(tick == 100000) {
      won  = true
      lost = true
    }

    lastPlayer  = playerZero
    lastProgram = programZero

    tick = tick + 1
  }

  def evaluateInner() {
    while(ip < program.opcodes.length) {
      if(won || lost || dead) return

      val ins = program.opcodes(ip)
      debugPrint("")
      debugPrint("dp = "+dp+", tape(dp) = "+(if(dp < 0 || dp >= tape.length) "X" else tape(dp)))
      debugPrint("ip = "+ip+", opcodes(ip) = "+Opcode.disassembleInstruction(program.opcodes(ip)))
      debugPrint("rc = ["+rc.mkString(", ")+"]")
      debugPrint(tape.map(x => "%02X".format(x)).reduce(_ + " " + _))
      ip = ip + 1
      ins match {
        case Opcode.IncData =>
          tape(dp) = (tape(dp) + 1 * polarity).toByte
          return
        case Opcode.DecData =>
          tape(dp) = (tape(dp) - 1 * polarity).toByte
          return
        case Opcode.IncPtr =>
          dp = dp - 1
          return
        case Opcode.DecPtr =>
          dp = dp + 1
          return
        case Opcode.NullOp =>
          return

        case Opcode.BeginLoop(t) =>
          if(tape(dp) == 0) ip = t
          return
        case Opcode.EndLoop  (t) =>
          if(tape(dp) != 0) ip = t
          return

        case Opcode.BeginRepeat(id) =>
          rc(id) = 0
        case Opcode.EndRepeat(i, c, id) =>
          rc(id) = rc(id) + 1
          if(rc(id) < c) ip = i
        case Opcode.EndOuter(i, c, id) =>
          if(rc(id) > 0) ip = i
          rc(id) = rc(id) - 1

        case _ => sys.error("unknown opcode")
      }
    }
  }
  def evaluate() {
    debugPrint("##### "+name)
    try {
      evaluateInner()
    } catch {
      case t: Throwable =>
        lost = true
        System.err.println("Error encountered while evaluating "+name)
        t.printStackTrace()
    }
  }
  def addTo(dp: Int, amount: Int) = tape(dp) = (tape(dp) + amount).toByte

  def hasLost = lost && !won
  def hasWon  = won  && !lost
  def hasTied = lost && won
  def isEnded = lost || won

  override def clone() = {
    val rt = ProgramState(name, program, polarity, util.Arrays.copyOf(tape, tape.length))

    rt.lost = lost
    rt.won  = won
    rt.dead = dead

    rt.tick = tick

    rt.dp = dp

    rt.ip      = ip
    rt.rc      = util.Arrays.copyOf(rc, rc.length)

    rt.lastPlayer  = lastPlayer
    rt.lastProgram = lastProgram

    rt
  }

  override def toString: String = "BFJoustState ("+name+", tape size = "+tape.length+", polarity = "+polarity+")"
}
object ProgramState {
  private val debug = false
  def debugPrint(s: => Object) = if(debug) println(s)

  def makeTape(length: Int) = {
    val array = new Array[Byte](length)
    array(0) = -128
    array(length - 1) = -128
    array
  }

  def generateTapes(name: String, program: Program) = {
    for(len <- 10 to 30;
        pol <- Seq(-1, 1)) yield ProgramState(name, program, pol, makeTape(len))
  }
}