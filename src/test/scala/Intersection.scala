import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Intersection extends AnyFunSuite {
  test("Intersection") {
    val program = TSProgram(Seq("src/test/typescript/Intersection.ts"))
    System.out.println(s"rua: ${program.>("extend")}")
    assert(TypeCompare(program.>("extend"), "(T, U) => (T & U)"))
    assert(TypeCompare(program.>("foo"), "(T & U) => void"))
  }
}

