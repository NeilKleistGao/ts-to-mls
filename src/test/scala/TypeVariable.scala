import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class TypeVariable extends AnyFunSuite {
  test("Type Variable") {
    val program = TSProgram(Seq("src/test/typescript/TypeVariable.ts"))
    System.out.println(s"rua: ${program.>("inc")}")
    assert(TypeCompare(program.>("inc"), "T => number where T <: number"))
    assert(TypeCompare(program.>("CC"), "class CC where T <: string"))
  }
}
