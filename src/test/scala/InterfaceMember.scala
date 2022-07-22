import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class InterfaceMember extends AnyFunSuite {
  test("Interface Member") {
    val program = TSProgram("src/test/typescript/InterfaceMember.ts")
    assert(TypeCompare(program.getType("IFoo"), "interface IFoo {\n\ta: string\n\tb: number => number\n\tc: boolean\n\td: string => void\n}"))
    assert(TypeCompare(program.getType("IFoo").>("a"), "string"))
    assert(TypeCompare(program.getType("IFoo").>("b"), "number => number"))
    assert(TypeCompare(program.getType("IFoo").>("c"), "boolean"))
    assert(TypeCompare(program.getType("IFoo").>("d"), "string => void"))
  }
}
