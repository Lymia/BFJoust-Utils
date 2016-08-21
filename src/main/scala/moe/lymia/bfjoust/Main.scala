package moe.lymia.bfjoust

import java.io.File
import java.io.PrintWriter

import moe.lymia.bfjoust.solver._
import moe.lymia.bfjoust.vm._

case class CLIArguments(command: CLIArguments => Unit,
                        // solveHill options
                        hill: File = null, output: File = null,
                        // arena battle options
                        leftProgram: File = null, rightProgram: File = null,
                        // testArena options
                        tapeLength: Int = 0, parity: Boolean = false, traceAll: Boolean = false,
                        disassemble: Boolean = false)

object Main {
  private val parser = new scopt.OptionParser[CLIArguments]("mvmm") {
    head("BFJoust")

    help("help").text("Displays this message.")

    cmd("solveHill").action((_, args) => args.copy(command = cmd_solveHill)).children(
      arg[File]("<hill>"       ).action((f, args) => args.copy(hill   = f)).hidden(),
      arg[File]("<output file>").action((f, args) => args.copy(output = f)).hidden()
    )
    cmd("testArena").action((_, args) => args.copy(command = cmd_testArena)).children(
      arg[File   ]("<left program>" ).action((f, args) => args.copy(leftProgram  = f)).hidden(),
      arg[File   ]("<right program>").action((f, args) => args.copy(rightProgram = f)).hidden(),
      arg[Int    ]("<tape length>"  ).action((i, args) => args.copy(tapeLength   = i)).hidden(),

      opt[Unit]("reverse-parity").abbr("r").action((_, args) => args.copy(parity      = true)),
      opt[Unit]("trace-all"     ).abbr("a").action((_, args) => args.copy(traceAll    = true)),
      opt[Unit]("disassemble"   ).abbr("d").action((_, args) => args.copy(disassemble = true))
    )
  }

  def loadProgram(file: File) = {
    System.out.println("Loading: "+file)
    val source = scala.io.Source.fromFile(file).mkString
    Compiler(file.getName, source) match {
      case Left(program) => program
      case Right(err) => sys.error(err)
    }
  }
  def loadProgramDirectory(file: File) = file.listFiles().filter(_.isFile).map(loadProgram)

  private def cmd_unknown(args: CLIArguments) = {
    println("Unknown command.")
  }
  private def cmd_solveHill(args: CLIArguments) = {
    val c = new Cursor(0, ParallelEvaluator(loadProgramDirectory(args.hill)))
    Solver.executeStrategy(c)

    val pw = new PrintWriter(args.output)
    pw.println(c.toString())
    pw.close()
  }
  private def cmd_testArena(args: CLIArguments) = {
    val battle = Battle(loadProgram(args.leftProgram), loadProgram(args.rightProgram),
                        args.tapeLength, args.parity)
    if(args.disassemble) {
      battle.leftCode .printProgram()
      battle.rightCode.printProgram()
    }
    while(battle.status == ArenaStatus.Running) battle.cycle(trace = true, args.traceAll)
    println("Result: "+battle.status)
  }

  def main(args: Array[String]) =
    parser.parse(args, CLIArguments(cmd_unknown)) match {
      case Some(args) => args.command(args)
      case None       =>
    }
}