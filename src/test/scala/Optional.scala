import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Optional extends AnyFunSuite {
  test("Optional") {
    val program = TSProgram(Seq("src/test/typescript/Optional.ts"))
    assert(TypeCompare(program.>("buildName"), "(string, string | undefined) => string"))
  }
}
