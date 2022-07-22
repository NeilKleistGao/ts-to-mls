import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Array extends AnyFunSuite {
  test("Array") {
    val program = TSProgram("src/test/typescript/Array.ts")
    assert(TypeCompare(program.getType("first"), "string[] => string"))
  }
}
