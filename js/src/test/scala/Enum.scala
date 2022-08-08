import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Enum extends AnyFunSuite {
  test("Enum") {
    val program = TSProgram(Enum.testFiles)
    assert(TSTypeTest(program.>("pass"), "(Color) => boolean"))
    assert(TSTypeTest(program.>("stop"), "Color"))
  }
}

object Enum {
  private val testFiles = TSTypeTest.tsPathes(Seq("Enum.ts"))
}