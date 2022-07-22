import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class HighOrderFunc extends AnyFunSuite {
  test("High Order Function") {
    val program = TSProgram("src/test/typescript/HighOrderFunc.ts")
    assert(TypeCompare(program.getType("h1"), "(number => number, number) => number"))
    assert(TypeCompare(program.getType("h2"), "string => string"))
    System.out.println(program.getType("h3"))
    assert(TypeCompare(program.getType("h3"), "(number => number, number => number) => (number => number)"))
  }
}
