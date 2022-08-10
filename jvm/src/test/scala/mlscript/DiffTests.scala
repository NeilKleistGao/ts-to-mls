package mlscript


import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap}
import scala.collection.immutable
import mlscript.utils._, shorthands._
import org.scalatest.time._
import org.scalatest.concurrent.{TimeLimitedTests, Signaler}


class DiffTests(dir: os.Path = DiffTests.defaultDir)
  extends org.scalatest.funsuite.AnyFunSuite
  with org.scalatest.ParallelTestExecution
  with TimeLimitedTests
{
  private val allFiles = os.walk(dir).filter(_.toIO.isFile)

  import DiffTests._

  private val files = allFiles.filter { file =>
      val fileName = file.baseName
      validExt(file.ext)
  }

  // Aggregate unstaged modified files to only run the tests on them, if there are any
  private val modified: Set[Str] =
    try os.proc("git", "status", "--porcelain", dir).call().out.lines().iterator.flatMap { gitStr =>
      println(" [git] " + gitStr)
      val prefix = gitStr.take(2)
      val filePath = gitStr.drop(3)
      val fileName = os.RelPath(filePath).baseName
      if (prefix =:= "A " || prefix =:= "M ") N else S(fileName) // disregard modified files that are staged
    }.toSet catch {
      case err: Throwable => System.err.println("/!\\ git command failed with: " + err)
      Set.empty
    }
  
  val timeLimit = TimeLimit
  
  override val defaultTestSignaler: Signaler = new Signaler {
    @annotation.nowarn("msg=method stop in class Thread is deprecated") def apply(testThread: Thread): Unit = {
      println(s"!! Test at $testThread has run out out time !! stopping...")
      // * Thread.stop() is considered bad practice because normally it's better to implement proper logic
      // * to terminate threads gracefully, avoiding leaving applications in a bad state.
      // * But here we DGAF since all the test is doing is runnign a type checker and some Node REPL,
      // * which would be a much bigger pain to make receptive to "gentle" interruption.
      // * It would feel extremely wrong to intersperse the pure type checker algorithms
      // * with ugly `Thread.isInterrupted` checks everywhere...
      testThread.stop()
    }
  }
  
  files.foreach { file => val fileName = file.baseName; test(fileName) {
    
    val buf = mutable.ArrayBuffer.empty[Char]
    buf ++= s"Processed  $fileName"
    
    // For some reason the color is sometimes wiped out when the line is later updated not in iTerm3:
    // println(s"${Console.CYAN}Processing $fileName${Console.RESET}... ")
    
    val beginTime = System.nanoTime()
    
    val outputMarker = "//│ "
    // val oldOutputMarker = "/// "
    
    val diffBegMarker = "<<<<<<<"
    val diffMidMarker = "======="
    val diffEndMarker = ">>>>>>>"
    
    val fileContents = os.read(file)
    val allLines = fileContents.splitSane('\n').toList
    val strw = new java.io.StringWriter
    val out = new java.io.PrintWriter(strw) {
      override def println(): Unit = print('\n') // to avoid inserting CRLF on Windows
    }
    var stdout = false
    def output(str: String) =
      // out.println(outputMarker + str)
      if (stdout) System.out.println(str) else
      str.splitSane('\n').foreach(l => out.println(outputMarker + l))
    
    val failures = mutable.Buffer.empty[Int]
    val unmergedChanges = mutable.Buffer.empty[Int]

    case class Mode(
      expectTypeErrors: Bool = false,
      expectWarnings: Bool = false,
      expectParseErrors: Bool = false,
      fixme: Bool = false,
      showParse: Bool = false,
      verbose: Bool = false,
      noSimplification: Bool = false,
      explainErrors: Bool = false,
      dbg: Bool = false,
      dbgSimplif: Bool = false,
      fullExceptionStack: Bool = false,
      stats: Bool = false,
      stdout: Bool = false,
      noExecution: Bool = false,
      noGeneration: Bool = false,
      showGeneratedJS: Bool = false,
      generateTsDeclarations: Bool = false,
      debugVariance: Bool = false,
      expectRuntimeErrors: Bool = false,
      expectCodeGenErrors: Bool = false,
      showRepl: Bool = false,
      allowEscape: Bool = false,
      // noProvs: Bool = false,
    ) {
      def isDebugging: Bool = dbg || dbgSimplif
    }
    val defaultMode = Mode()
    
    var allowTypeErrors = false
    var showRelativeLineNums = false
    var noJavaScript = false
    var noProvs = false
    var allowRuntimeErrors = false
    
    def rec(lines: List[String], mode: Mode): Unit = lines match {
      case "" :: Nil =>
      case line :: ls if line.startsWith(":") =>
        out.println(line)
        val newMode = line.tail.takeWhile(!_.isWhitespace) match {
          case "e" => mode.copy(expectTypeErrors = true)
          case "w" => mode.copy(expectWarnings = true)
          case "pe" => mode.copy(expectParseErrors = true)
          case "p" => mode.copy(showParse = true)
          case "d" => mode.copy(dbg = true)
          case "ds" => mode.copy(dbgSimplif = true)
          case "s" => mode.copy(fullExceptionStack = true)
          case "v" | "verbose" => mode.copy(verbose = true)
          case "ex" | "explain" => mode.copy(expectTypeErrors = true, explainErrors = true)
          case "ns" | "no-simpl" => mode.copy(noSimplification = true)
          case "stats" => mode.copy(stats = true)
          case "stdout" => mode.copy(stdout = true)
          case "AllowTypeErrors" => allowTypeErrors = true; mode
          case "AllowRuntimeErrors" => allowRuntimeErrors = true; mode
          case "ShowRelativeLineNums" => showRelativeLineNums = true; mode
          case "NoJS" => noJavaScript = true; mode
          case "NoProvs" => noProvs = true; mode
          case "ne" => mode.copy(noExecution = true)
          case "ng" => mode.copy(noGeneration = true)
          case "js" => mode.copy(showGeneratedJS = true)
          case "ts" => mode.copy(generateTsDeclarations = true)
          case "dv" => mode.copy(debugVariance = true)
          case "ge" => mode.copy(expectCodeGenErrors = true)
          case "re" => mode.copy(expectRuntimeErrors = true)
          case "ShowRepl" => mode.copy(showRepl = true)
          case "escape" => mode.copy(allowEscape = true)
          case _ =>
            failures += allLines.size - lines.size
            output("/!\\ Unrecognized option " + line)
            mode
        }
        rec(ls, newMode)
      case line :: ls if line.startsWith("// FIXME") || line.startsWith("// TODO") =>
        out.println(line)
        rec(ls, mode.copy(fixme = true))
      case line :: ls if line.startsWith(outputMarker) //|| line.startsWith(oldOutputMarker)
        => rec(ls, defaultMode)
      case line :: ls if line.isEmpty =>
        out.println(line)
        rec(ls, defaultMode)
      case line :: ls if line.startsWith("//") =>
        out.println(line)
        rec(ls, mode)
      case line :: ls if line.startsWith(diffBegMarker) => // Check if there are unmerged git conflicts
        val diff = ls.takeWhile(l => !l.startsWith(diffEndMarker))
        assert(diff.exists(_.startsWith(diffMidMarker)), diff)
        val rest = ls.drop(diff.length)
        val hdo = rest.headOption
        assert(hdo.exists(_.startsWith(diffEndMarker)), hdo)
        val blankLines = diff.count(_.isEmpty)
        val hasBlankLines = diff.exists(_.isEmpty)
        if (diff.forall(l => l.startsWith(outputMarker) || l.startsWith(diffMidMarker) || l.isEmpty)) {
          for (_ <- 1 to blankLines) out.println()
        } else {
          unmergedChanges += allLines.size - lines.size + 1
          out.println(diffBegMarker)
          diff.foreach(out.println)
          out.println(diffEndMarker)
        }
        rec(rest.tail, if (hasBlankLines) defaultMode else mode)
      // process block of text and show output - type, expressions, errors
      case l :: ls => {}
      case Nil =>
    }

    try rec(allLines, defaultMode) finally {
      out.close()
    }
    val testFailed = failures.nonEmpty || unmergedChanges.nonEmpty
    val result = strw.toString
    val endTime = System.nanoTime()
    val timeStr = (((endTime - beginTime) / 1000 / 100).toDouble / 10.0).toString
    val testColor = if (testFailed) Console.RED else Console.GREEN
    buf ++= s"${" " * (30 - fileName.size)}${testColor}${
      " " * (6 - timeStr.size)}$timeStr  ms${Console.RESET}"
    if (result =/= fileContents) {
      buf ++= s"\n! Updated $file"
      os.write.over(file, result)
    }
    println(buf.mkString)
    if (testFailed)
      if (unmergedChanges.nonEmpty)
        fail(s"Unmerged non-output changes around: " + unmergedChanges.map("l."+_).mkString(", "))
      else fail(s"Unexpected diagnostics (or lack thereof) at: " + failures.map("l."+_).mkString(", "))
    
  }}
}

object DiffTests {
  
  private val TimeLimit =
    if (sys.env.get("CI").isDefined) Span(25, Seconds)
    else Span(5, Seconds)
  
  private val defaultDir = os.pwd/"shared"/"src"/"test"/"diff"
  
  private val validExt = Set("fun", "mls")
  
  // Allow overriding which specific tests to run, sometimes easier for development:
  private val focused = Set[Str](
    // "Ascribe",
    // "Repro",
    // "RecursiveTypes",
    // "Simple",
    // "Inherit",
    // "Basics",
    // "Paper",
    // "Negations",
    // "RecFuns",
    // "With",
    // "Annoying",
    // "Tony",
    // "Lists",
    // "Traits",
    // "BadTraits",
    // "TraitMatching",
    // "Subsume",
    // "Methods",
  )
}
