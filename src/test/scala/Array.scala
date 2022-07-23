import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Array extends AnyFunSuite {
  test("Array") {
    val program = TSProgram("src/test/typescript/Array.ts")
    assert(TypeCompare(program.getType("first"), "string[] => string"))
    System.out.println(program.getType("getZero3").toString())
    assert(TypeCompare(program.getType("getZero3"), "number[]"))
  }
}
