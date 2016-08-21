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
    case Success(nodes, _)   => Left(nodes)
    case NoSuccess(err,next) => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}