import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class NameSpace extends AnyFunSuite {
  test("Name Space") {
    val program = TSProgram("src/test/typescript/NameSpace.ts")
    assert(TypeCompare(program.getType("N1"), "namespace N1"))
    assert(TypeCompare(program.getType("N1").>("f"), "any => number"))
    assert(TypeCompare(program.getType("N1").>("C").>("f"), "void"))
    assert(TypeCompare(program.getType("N1").>("N2").>("fff"), "boolean => number"))
  }
}
