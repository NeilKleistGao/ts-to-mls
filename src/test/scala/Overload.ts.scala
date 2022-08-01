import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Overload extends AnyFunSuite {
  test("Overload") {
    val program = TSProgram(Seq("src/test/typescript/Overload.ts"))
    assert(TypeCompare(program.>("f"), "(number) => number & ((string) => string)"))
  }
}

