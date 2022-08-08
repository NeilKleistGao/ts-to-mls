import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class MultiFiles extends AnyFunSuite {
  test("Multiple Files") {
    val program = TSProgram(Seq("js/src/test/typescript/Multi1.ts", "js/src/test/typescript/Multi2.ts"))
    assert(TypeCompare(program.>("multi1"), "(number) => number"))
    assert(TypeCompare(program.>("multi2"), "(string) => string"))
  }
}
