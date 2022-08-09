package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Optional extends AnyFunSuite {
  test("Optional") {
    val program = TSProgram(Optional.testFiles)
    assert(TSTypeTest(program.>("buildName"), "(string, string | undefined) => string"))
    assert(TSTypeTest(program.>("buildName2"), "(string, string | undefined) => string"))
    assert(TSTypeTest(program.>("buildName3"), "(string, any[]) => string"))

    assert(TSTypeTest(program.>("SquareConfig").>("color"), "string | undefined"))
    assert(TSTypeTest(program.>("SquareConfig").>("width"), "number | undefined"))

    assert(TSTypeTest(program.>("did"), "(number, ((number) => number) | undefined) => number"))
    assert(TSTypeTest(program.>("getOrElse"), "(object[] | undefined) => object"))
    assert(TSTypeTest(program.>("testABC"), "(ABC | undefined) => void"))
    assert(TSTypeTest(program.>("testSquareConfig"), "(SquareConfig | undefined) => void"))
    assert(TSTypeTest(program.>("err"), "([number, string] | undefined) => void"))
    assert(TSTypeTest(program.>("toStr"), "(number | boolean | undefined) => string"))
    assert(TSTypeTest(program.>("boo"), "(T' & U' | undefined) => void"))
    assert(TSTypeTest(program.>("boom"), "(B<never> | undefined) => any"))
  }
}

object Optional {
  private val testFiles = TSTypeTest.tsPathes(Seq("Optional.ts"))
}