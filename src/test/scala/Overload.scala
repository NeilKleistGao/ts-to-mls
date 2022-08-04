import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Overload extends AnyFunSuite {
  test("Overload") {
    val program = TSProgram(Seq("src/test/typescript/Overload.ts"))
    assert(TypeCompare(program.>("f"), "(number) => number & ((string) => string)"))
    assert(TypeCompare(program.>("app"), "((number) => void, number) => void & (((string) => void, string) => void)"))
    assert(TypeCompare(program.>("create"), "(number) => number & ((boolean) => boolean)"))
    assert(TypeCompare(program.>("g0"), "(string[]) => string & ((object[]) => object)"))
  }
}

