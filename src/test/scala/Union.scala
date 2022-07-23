import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Union extends AnyFunSuite {
  test("Union") {
    val program = TSProgram("src/test/typescript/Union.ts")
    assert(TypeCompare(program.getType("getString"), "(string | number | boolean) => string"))
  }
}
