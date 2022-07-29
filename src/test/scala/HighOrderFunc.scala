import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class HighOrderFunc extends AnyFunSuite {
  test("High Order Function") {
    val program = TSProgram(Seq("src/test/typescript/HighOrderFunc.ts"))
    assert(TypeCompare(program.>("h1"), "(number => number, number) => number"))
    assert(TypeCompare(program.>("h2"), "string => string"))
    assert(TypeCompare(program.>("h3"), "(number => number, number => number) => (number => number)"))
  }
}
