import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class TypeVariable extends AnyFunSuite {
  test("Type Variable") {
    val program = TSProgram("src/test/typescript/TypeVariable.ts")
    assert(TypeCompare(program.getType("inc"), "T => number where T <: number"))
  }
}
