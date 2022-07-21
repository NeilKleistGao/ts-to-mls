import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class InterfaceMember extends AnyFunSuite {
  test("Interface Member") {
    val program = TSProgram("src/test/typescript/InterfaceMember.ts")
    assert(TypeCompare(program.getType("IFoo"), "interface IFoo"))
    assert(TypeCompare(program.getType("IFoo").>("b"), "number => number"))
    assert(TypeCompare(program.getType("IFoo").>("c"), "boolean"))
    assert(TypeCompare(program.getType("IFoo").>("d"), "string => void"))

    try {
        program.getType("IFoo").>("a")
        assert(false)
    }
    catch {
        case e: java.lang.Exception => assert(e.getMessage().equals("Field \"a\" not found."))
    }
  }
}
