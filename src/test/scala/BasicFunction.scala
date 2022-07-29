import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class BasicFunction extends AnyFunSuite {
  test("Basic Function") {
    val program = TSProgram(Seq("src/test/typescript/BasicFunctions.ts"))
    assert(TypeCompare(program.>("hello"), "void"))
    assert(TypeCompare(program.>("add"), "(number, number) => number"))
    assert(TypeCompare(program.>("sub"), "(number, number) => number"))
    assert(TypeCompare(program.>("foo"), "number"))
    assert(TypeCompare(program.>("id"), "any => any"))
    assert(TypeCompare(program.>("odd"), "number => boolean"))
    assert(TypeCompare(program.>("isnull"), "any => boolean"))
    assert(TypeCompare(program.>("bar"), "any"))
  }
}
