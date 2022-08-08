import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class MultiFiles extends AnyFunSuite {
  test("Multiple Files") {
    val program = TSProgram(MultiFiles.testFiles)
    assert(TSTypeTest(program.>("multi1"), "(number) => number"))
    assert(TSTypeTest(program.>("multi2"), "(string) => string"))
  }
}

object MultiFiles {
  private val testFiles = TSTypeTest.tsPathes(Seq("Multi1.ts", "Multi2.ts"))
}