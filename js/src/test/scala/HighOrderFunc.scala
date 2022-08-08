import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class HighOrderFunc extends AnyFunSuite {
  test("High Order Function") {
    val program = TSProgram(HighOrderFunc.testFiles)
    assert(TSTypeTest(program.>("h1"), "((number) => number, number) => number"))
    assert(TSTypeTest(program.>("h2"), "(string) => string"))
    assert(TSTypeTest(program.>("h3"), "((number) => number, (number) => number) => (number) => number"))
  }

  test("High Order Convert") {
    import mlscript._

    val program = TSProgram(HighOrderFunc.testFiles)

    program.getMLSType("h1") match {
      case Function(lhs, rhs) => lhs match {
        case Function(lhs2, rhs2) => rhs2 match {
          case TypeName(name) => assert(name.equals("number"))
          case _ => assert(false)
        }
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }
}

object HighOrderFunc {
  private val testFiles = TSTypeTest.tsPathes(Seq("HighOrderFunc.ts"))
}