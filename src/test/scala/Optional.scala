import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Optional extends AnyFunSuite {
  test("Optional") {
    val program = TSProgram(Seq("src/test/typescript/Optional.ts"))
    System.out.println(s"rua: ${program.>("buildName")}")
    assert(TypeCompare(program.>("buildName"), "(String, String | undefined) => string"))
  }
}
