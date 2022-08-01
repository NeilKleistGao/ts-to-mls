import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class InterfaceMember extends AnyFunSuite {
  test("Interface Member") {
    val program = TSProgram(Seq("src/test/typescript/InterfaceMember.ts"))
    assert(TypeCompare(program.>("IFoo"), "interface IFoo {\n\ta: string\n\tb: (number) => number\n\tc: boolean\n\td: (string) => void\n}"))
    assert(TypeCompare(program.>("IFoo").>("a"), "string"))
    assert(TypeCompare(program.>("IFoo").>("b"), "(number) => number"))
    assert(TypeCompare(program.>("IFoo").>("c"), "boolean"))
    assert(TypeCompare(program.>("IFoo").>("d"), "(string) => void"))

    assert(TypeCompare(program.>("II"), "interface II {\n\ttest: (T') => number\n} where T' <: number"))
    assert(TypeCompare(program.>("II").>("test"), "(T') => number"))
  }
}
