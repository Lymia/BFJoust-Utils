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

package moe.lymia.bfjoust.vm

import java.util

sealed trait TempOpcode
sealed trait Opcode extends TempOpcode
object Opcode {
  case object IncData extends Opcode
  case object DecData extends Opcode
  case object IncPtr  extends Opcode
  case object DecPtr  extends Opcode
  case object NullOp  extends Opcode

  case class BeginLoop  (i: Int                     ) extends Opcode
  case class EndLoop    (i: Int                     ) extends Opcode
  case class BeginRepeat(                    id: Int) extends Opcode
  case class EndRepeat  (i: Int, count: Int, id: Int) extends Opcode
  case class EndOuter   (i: Int            , id: Int) extends Opcode

  case class ApplyLabel(fn: Int => Opcode, label: Label) extends TempOpcode
  class Label() extends TempOpcode

  def disassembleInstruction(op: Opcode) = op match {
    case Opcode.BeginLoop(t) => "BeginLoop @"+t
    case Opcode.EndLoop  (t) => "EndLoop @"+t

    case Opcode.BeginRepeat(      id) => "BeginRepeat["+id+"]"
    case Opcode.EndRepeat  (t, c, id) => "EndRepeat["+id+"] "+c+" @"+t
    case Opcode.EndOuter   (t,    id) => "EndOuter["+id+"]  @"+t

    case x => x.toString
  }
  def disassemble(ops: Seq[Opcode]) = {
    val fmt = " %"+math.max(4, (ops.length - 1).toString.length)+"d: %s"
    for((op, i) <- ops.zipWithIndex) {
      println(fmt.format(i, disassembleInstruction(op)))
    }
  }
}
case class Program(name: String, opcodes: IndexedSeq[Opcode], idCount: Int) {
  def printProgram() = {
    println("=== "+name+" ===")
    Opcode.disassemble(opcodes)
  }
}

final class BytecodeEvaluator private (val program: Program, private var ip: Int, private var rc: Array[Int]) {
  def cycle(test: Boolean, trace: Boolean = false, traceLead: String = ""): ArenaAction = {
    while(ip < program.opcodes.length) {
      val ins = program.opcodes(ip)
      if(trace) println(traceLead+"ip = "+ip+", opcodes(ip) = "+Opcode.disassembleInstruction(ins)+
                                  ", rc = "+util.Arrays.toString(rc))
      ip = ip + 1
      ins match {
        case Opcode.IncData =>
          return ArenaAction.IncData
        case Opcode.DecData =>
          return ArenaAction.DecData
        case Opcode.IncPtr =>
          return ArenaAction.IncPtr
        case Opcode.DecPtr =>
          return ArenaAction.DecPtr
        case Opcode.NullOp =>
          return ArenaAction.NullOp

        case Opcode.BeginLoop(t) =>
          if(!test) ip = t
          return ArenaAction.NullOp
        case Opcode.EndLoop  (t) =>
          if(test) ip = t
          return ArenaAction.NullOp

        case Opcode.BeginRepeat(id) =>
          rc(id) = 1
        case Opcode.EndRepeat(i, c, id) =>
          if(rc(id) < c) {
            ip = i
            rc(id) = rc(id) + 1
          }
        case Opcode.EndOuter(i, id) =>
          rc(id) = rc(id) - 1
          if(rc(id) > 0) ip = i

        case _ => sys.error("unknown opcode")
      }
    }
    ArenaAction.NullOp
  }

  override def clone() = new BytecodeEvaluator(program, ip, util.Arrays.copyOf(rc, rc.length))
  override def toString: String = "BytecodeEvaluator[@"+program.name+"]"
}
object BytecodeEvaluator {
  def apply(program: Program) = new BytecodeEvaluator(program, 0, new Array[Int](program.idCount))
}