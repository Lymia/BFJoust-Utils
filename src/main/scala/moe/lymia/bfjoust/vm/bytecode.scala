package moe.lymia.bfjoust.vm

import moe.lymia.bfjoust.vm.Opcode.Label

import scala.collection.mutable

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
  case class EndOuter   (i: Int, count: Int, id: Int) extends Opcode

  case class ApplyLabel(fn: Int => Opcode, label: Label) extends TempOpcode
  class Label() extends TempOpcode

  def disassembleInstruction(op: Opcode) = op match {
    case Opcode.BeginLoop(t) => "BeginLoop @"+t
    case Opcode.EndLoop  (t) => "EndLoop @"+t

    case Opcode.BeginRepeat(      id) => "BeginRepeat["+id+"]"
    case Opcode.EndRepeat  (t, c, id) => "EndRepeat["+id+"] "+c+" @"+t
    case Opcode.EndOuter   (t, c, id) => "EndOuter["+id+"] "+c+" @"+t

    case x => x.toString
  }
  def disassemble(ops: Seq[Opcode]) = {
    val fmt = " %"+math.max(4, (ops.length - 1).toString.length)+"d: %s"
    for((op, i) <- ops.zipWithIndex) {
      println(fmt.format(i, disassembleInstruction(op)))
    }
  }
}

case class Program(opcodes: IndexedSeq[Opcode], idCount: Int)

object Compiler {
  private def compile(ast: Seq[AST.Instruction]) = {
    def loop(ast: Seq[AST.Instruction], currentId: Int,
             loopLabels: List[(Label, Label, Int, Int)]): Seq[TempOpcode] =
      ast flatMap {
        case AST.IncData => Seq(Opcode.IncData)
        case AST.DecData => Seq(Opcode.DecData)
        case AST.IncPtr  => Seq(Opcode.IncPtr )
        case AST.DecPtr  => Seq(Opcode.DecPtr )
        case AST.NullOp  => Seq(Opcode.NullOp )

        case AST.Repeat(i, l) =>
          val beginLabel = new Opcode.Label()
          Seq(Opcode.BeginRepeat(currentId), beginLabel) ++
          loop(i, currentId + 1, loopLabels) ++
          Seq(Opcode.ApplyLabel(i => Opcode.EndRepeat(i, l, currentId), beginLabel))
        case AST.Loop(i) =>
          val beginLabel = new Opcode.Label()
          val endLabel   = new Opcode.Label()
          Seq(Opcode.ApplyLabel(i => Opcode.BeginLoop(i), endLabel  ), beginLabel) ++
          loop(i, currentId, loopLabels) ++
          Seq(Opcode.ApplyLabel(i => Opcode.EndLoop  (i), beginLabel), endLabel  )

        case AST.Outer(i, l) =>
          val beginLabel = new Opcode.Label()
          val innerLabel = new Opcode.Label()
          Seq(Opcode.BeginRepeat(currentId), beginLabel) ++
          loop(i, currentId + 1, (beginLabel, innerLabel, currentId, l) :: loopLabels) ++
          Seq(Opcode.ApplyLabel(i => Opcode.EndOuter(i, l, currentId), innerLabel))
        case AST.Inner(i) =>
          val (beginLabel, innerLabel, id, l) = loopLabels.head
          Seq(Opcode.ApplyLabel(i => Opcode.EndRepeat(i, l, id), beginLabel)) ++
          loop(i, currentId, loopLabels.tail) ++
          Seq(innerLabel)

        case i => sys.error("Unknown instruction: "+i)
      }
    loop(ast, 0, List())
  }

  private def resolveLabels(ops: Seq[TempOpcode]): Seq[Opcode] = {
    val posMap = new mutable.HashMap[Opcode.Label, Int]

    var p = 0
    for(o <- ops) o match {
      case l: Label => posMap.put(l, p)
      case _        => p = p + 1
    }

    ops.filter(!_.isInstanceOf[Label]).map {
      case o: Opcode => o
      case Opcode.ApplyLabel(fn, l) => fn(posMap(l))
      case _ => sys.error("failed to resolve labels")
    }
  }

  private def findIdSize(ops: Seq[Opcode]) = ops.map {
    case Opcode.BeginRepeat(      id) => id
    case Opcode.EndRepeat  (_, _, id) => id
    case Opcode.EndOuter   (_, _, id) => id
    case _                            => -1
  }.max + 1

  def apply(ast: Seq[AST.Instruction]) = {
    val compiled = resolveLabels(compile(ast))
    Program(compiled.toIndexedSeq, findIdSize(compiled))
  }
}