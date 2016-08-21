package moe.lymia.bfjoust.vm

import scala.collection.mutable

object Compiler {
  private def processProgramInner(instructions: Seq[AST.Instruction]): (Seq[AST.Instruction], Int) = {
    val data = for(i <- instructions) yield i match {
      case AST.Loop(newIns) =>
        val (ni, ct) = processProgramInner(newIns)
        (AST.Loop(ni), ct)
      case AST.Repeat(newIns, c) =>
        val (ni, ct) = processProgramInner(newIns)
        if(ct > 0) (AST.Outer (ni, c), ct - 1)
        else       (AST.Repeat(ni, c), 0)
      case AST.Inner(newIns) =>
        val (ni, ct) = processProgramInner(newIns)
        (AST.Inner(ni), ct + 1)
      case AST.Outer(_, _) => sys.error("AST.Outer in source parse.")
      case x => (x, 0)
    }
    if(data.count(_._2 > 0) > 1) sys.error("Multiple {} enclosed in same ()% scope.")
    (data.map(_._1), data.map(_._2).sum)
  }
  private def removeEmptyLoopsInner(instructions: Seq[AST.Instruction]): Seq[AST.Instruction] =
    instructions.flatMap {
      case AST.Repeat(Seq(), _) | AST.Repeat(_, 0) => Seq()

      case AST.Repeat(Seq(AST.Inner(x)), _) => x
      case AST.Loop(i) => Seq(AST.Loop(removeEmptyLoopsInner(i)))
      case AST.Repeat(i, c) => Seq(AST.Repeat(removeEmptyLoopsInner(i), c))
      case AST.Inner(i) => Seq(AST.Inner(removeEmptyLoopsInner(i)))
      case x => Seq(x)
    }
  def processAST(instructions: Seq[AST.Instruction]) = {
    var ci = instructions
    while({
      val ni = removeEmptyLoopsInner(ci)
      if(ni == ci) false
      else {
        ci = ni
        true
      }
    }) {}
    val (fi, ct) = processProgramInner(ci)
    if(ct != 0) sys.error("Too many {}")
    fi
  }

  private def compile(ast: Seq[AST.Instruction]) = {
    def loop(ast: Seq[AST.Instruction], currentId: Int,
             loopLabels: List[(Opcode.Label, Opcode.Label, Int, Int)]): Seq[TempOpcode] =
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
          Seq(Opcode.ApplyLabel(i => Opcode.EndOuter(i, currentId), innerLabel))
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
      case l: Opcode.Label =>
        if(posMap.contains(l))
          sys.error("internal error: repeat label "+l+" (was at pos "+posMap(l)+", new at "+p+")")
        posMap.put(l, p)
      case _               => p = p + 1
    }

    ops.filter(!_.isInstanceOf[Opcode.Label]).map {
      case o: Opcode => o
      case Opcode.ApplyLabel(fn, l) => fn(posMap(l))
      case _ => sys.error("failed to resolve labels")
    }
  }

  private def findIdSize(ops: Seq[Opcode]) = (-1 +: ops.map {
    case Opcode.BeginRepeat(      id) => id
    case Opcode.EndRepeat  (_, _, id) => id
    case Opcode.EndOuter   (_,    id) => id
    case _                            => -1
  }).max + 1

  def apply(name: String, ast: Seq[AST.Instruction]): Either[Program, String] = try {
    val compiled = resolveLabels(compile(processAST(ast)))
    Left(Program(name, compiled.toIndexedSeq, findIdSize(compiled)))
  } catch {
    case t: Throwable => Right("Error while compiling "+name+": "+t.getClass+" - "+t.getMessage)
  }
  def apply(name: String, program: String): Either[Program, String] =
    Parser(program).left.flatMap(x => Compiler(name, x))
}
