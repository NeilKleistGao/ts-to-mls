import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class BasicFunction extends AnyFunSuite {
  test("Basic Function") {
    val program = TSProgram("src/test/typescript/BasicFunctions.ts")
    assert(TypeCompare(program.getType("hello"), "void"))
    assert(TypeCompare(program.getType("add"), "(number, number) => number"))
    assert(TypeCompare(program.getType("sub"), "(number, number) => number"))
    assert(TypeCompare(program.getType("foo"), "number"))
    assert(TypeCompare(program.getType("id"), "any => any"))
    assert(TypeCompare(program.getType("odd"), "number => boolean"))
    assert(TypeCompare(program.getType("isnull"), "any => boolean"))
    assert(TypeCompare(program.getType("bar"), "any"))
  }
}
