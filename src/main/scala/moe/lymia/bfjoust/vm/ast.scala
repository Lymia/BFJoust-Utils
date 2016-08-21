package moe.lymia.bfjoust.vm

import language.postfixOps

object AST {
  sealed trait TempInstruction
  sealed trait Instruction extends TempInstruction

  case object UnkChar extends TempInstruction { override def toString = "!" }

  case object IncData extends Instruction { override def toString = "+" }
  case object DecData extends Instruction { override def toString = "-" }
  case object IncPtr  extends Instruction { override def toString = ">" }
  case object DecPtr  extends Instruction { override def toString = "<" }
  case object NullOp  extends Instruction { override def toString = "." }

  case class Loop  (instructions: Seq[Instruction]) extends Instruction {
    override def toString = "[" + instructions.mkString("") + "]"
  }
  case class Inner (instructions: Seq[Instruction]) extends Instruction {
    override def toString = "{" + instructions.mkString("") + "}"
  }
  case class Outer (instructions: Seq[Instruction], count: Int) extends Instruction {
    override def toString = "(" + instructions.mkString("") + ")%" + count
  }
  case class Repeat(instructions: Seq[Instruction], count: Int) extends Instruction {
    override def toString = "(" + instructions.mkString("") + ")*" + count
  }
}

object Parser extends scala.util.parsing.combinator.RegexParsers {
  override val whiteSpace = "[^\\[\\](){}+><.*%0-9-]+".r

  def basicInstructions = ("+" ^^^ AST.IncData) | ("-" ^^^ AST.DecData) | (">" ^^^ AST.IncPtr ) |
                          ("<" ^^^ AST.DecPtr ) | ("." ^^^ AST.NullOp )
  def number = ("[0-9]+".r ^^ {_.toInt}) | ("-[0-9]+".r ^^^ 100000)
  def loopTypes = (("[" ~> instructionSet <~ "]") ^^ AST.Loop ) |
                  (("{" ~> instructionSet <~ "}") ^^ AST.Inner) |
                  (("(" ~> instructionSet <~ ")") ~ ("[%*]".r ~> number) ^^ { case a ~ b => AST.Repeat(a, b) }) |
                  (("(" ~> instructionSet <~ ")") ^^^ AST.UnkChar)
  def instruction: Parser[AST.TempInstruction] = basicInstructions | loopTypes | ("[0-9*%]+".r ^^^ AST.UnkChar)
  def instructionSet: Parser[Seq[AST.Instruction]] =
    (instruction *) ^^ {_.filter(_.isInstanceOf[AST.Instruction]).map(_.asInstanceOf[AST.Instruction])}

  def apply(s: String) = parseAll(instructionSet, s) match {
    case Success(nodes, _)   => try {
      Left(ASTProcessor.processProgram(nodes))
    } catch {
      case t: Throwable => Right(t.getClass+" during parsing: "+t.getMessage)
    }
    case NoSuccess(err,next) => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}

object ASTProcessor {
  private def processProgramInner(instructions: Seq[AST.Instruction]): (Seq[AST.Instruction], Int) = {
    if(instructions.count(_.isInstanceOf[AST.Inner]) > 1) sys.error("{} in same scope as {} in program.")
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
      case AST.Outer(_, _) => sys.error("LoopOuter in source parse.")
      case x => (x, 0)
    }
    (data.map(_._1), data.map(_._2).sum)
  }
  private def removeEmptyLoopsInner(instructions: Seq[AST.Instruction]): Seq[AST.Instruction] =
    instructions.filter(_ match {
      case AST.Loop(Seq()) => false
      case AST.Repeat(Seq(), _) => false
      case AST.Repeat(_, 0) => false
      case _ => true
    }).flatMap {
      case AST.Loop(newIns) => Seq(AST.Loop(removeEmptyLoopsInner(newIns)))
      case AST.Repeat(Seq(AST.Inner(x)), _) => x
      case AST.Repeat(newIns, c) => Seq(AST.Repeat(removeEmptyLoopsInner(newIns), c))
      case AST.Inner(newIns) => Seq(AST.Inner(removeEmptyLoopsInner(newIns)))
      case x => Seq(x)
    }
  def processProgram(instructions: Seq[AST.Instruction]) = {
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
}