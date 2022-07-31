import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Array extends AnyFunSuite {
  test("Array") {
    val program = TSProgram(Seq("src/test/typescript/Array.ts"))
    assert(TypeCompare(program.>("first"), "(string[]) => string"))
    assert(TypeCompare(program.>("getZero3"), "number[]"))
    assert(TypeCompare(program.>("first2"), "(((number) => number)[]) => (number) => number"))
  }
}
