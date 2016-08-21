package moe.lymia.yume.bfjoust

import language.postfixOps

sealed trait BFJoustInstruction
case object UnkChar extends BFJoustInstruction { override def toString = "!" }
case object IncData extends BFJoustInstruction { override def toString = "+" }
case object DecData extends BFJoustInstruction { override def toString = "-" }
case object IncPtr  extends BFJoustInstruction { override def toString = ">" }
case object DecPtr  extends BFJoustInstruction { override def toString = "<" }
case object NullOp  extends BFJoustInstruction { override def toString = "." }

case class Loop  (instructions: Seq[BFJoustInstruction]) extends BFJoustInstruction {
  override def toString = "[" + instructions.mkString("") + "]"
}
case class Inner (instructions: Seq[BFJoustInstruction]) extends BFJoustInstruction {
  override def toString = "{" + instructions.mkString("") + "}"
}
case class Outer (instructions: Seq[BFJoustInstruction], count: Int) extends BFJoustInstruction {
  override def toString = "(" + instructions.mkString("") + ")%" + count
}
case class Repeat(instructions: Seq[BFJoustInstruction], count: Int) extends BFJoustInstruction {
  override def toString = "(" + instructions.mkString("") + ")*" + count
}

object Parser extends scala.util.parsing.combinator.RegexParsers {
  override val whiteSpace = "[^\\[\\](){}+><.*%0-9-]+".r

  def basicInstructions = ("+" ^^^ IncData) | ("-" ^^^ DecData) | (">" ^^^ IncPtr ) |
                          ("<" ^^^ DecPtr ) | ("." ^^^ NullOp )
  def number = ("[0-9]+".r ^^ {_.toInt}) | ("-[0-9]+".r ^^^ 100000)
  def loopTypes = (("[" ~> instructionSet <~ "]") ^^ Loop ) |
                  (("{" ~> instructionSet <~ "}") ^^ Inner) |
                  (("(" ~> instructionSet <~ ")") ~ ("[%*]".r ~> number) ^^ { case a ~ b => Repeat(a, b) }) |
                  (("(" ~> instructionSet <~ ")") ^^^ UnkChar)
  def instruction: Parser[BFJoustInstruction] = basicInstructions | loopTypes | ("[0-9*%]+".r ^^^ UnkChar)
  def instructionSet: Parser[Seq[BFJoustInstruction]] = (instruction *) ^^ {_.filter(_ != UnkChar)}

  def apply(s: String) = parseAll(instructionSet, s) match {
    case Success(nodes, _)   => try {
      Left(Cleanup.processProgram(nodes))
    } catch {
      case t: Throwable => Right(t.getClass+" during parsing: "+t.getMessage)
    }
    case NoSuccess(err,next) => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}

object Cleanup {
  private def processProgramInner(instructions: Seq[BFJoustInstruction]): (Seq[BFJoustInstruction], Int) = {
    if(instructions.count(_.isInstanceOf[Inner]) > 1) sys.error("{} in same scope as {} in program.")
    val data = for(i <- instructions) yield i match {
      case Loop(newIns) =>
        val (ni, ct) = processProgramInner(newIns)
        (Loop(ni), ct)
      case Repeat(newIns, c) =>
        val (ni, ct) = processProgramInner(newIns)
        if(ct > 0) (Outer (ni, c), ct - 1)
        else       (Repeat(ni, c), 0)
      case Inner(newIns) =>
        val (ni, ct) = processProgramInner(newIns)
        (Inner(ni), ct + 1)
      case Outer(_, _) => sys.error("LoopOuter in source parse.")
      case x => (x, 0)
    }
    (data.map(_._1), data.map(_._2).sum)
  }
  private def removeEmptyLoopsInner(instructions: Seq[BFJoustInstruction]): Seq[BFJoustInstruction] =
    instructions.filter(_ match {
      case Loop(Seq()) => false
      case Repeat(Seq(), _) => false
      case Repeat(_, 0) => false
      case _ => true
    }).flatMap {
      case Loop(newIns) => Seq(Loop(removeEmptyLoopsInner(newIns)))
      case Repeat(Seq(Inner(x)), _) => x
      case Repeat(newIns, c) => Seq(Repeat(removeEmptyLoopsInner(newIns), c))
      case Inner(newIns) => Seq(Inner(removeEmptyLoopsInner(newIns)))
      case x => Seq(x)
    }
  def processProgram(instructions: Seq[BFJoustInstruction]) = {
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