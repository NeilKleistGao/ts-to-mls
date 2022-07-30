import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Tuple extends AnyFunSuite {
  test("Tuple") {
    val program = TSProgram(Seq("src/test/typescript/Tuple.ts"))
    assert(TypeCompare(program.>("key"), "[string, boolean] => string"))
    assert(TypeCompare(program.>("value"), "[string, boolean] => boolean"))
    assert(TypeCompare(program.>("third"), "[number, number, number] => number"))
    assert(TypeCompare(program.>("vec2"), "(number, number) => [number, number]"))
  }
}
