package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class HighOrderFunc extends AnyFunSuite {
  test("High Order Function") {
    val program = TSProgram(HighOrderFunc.testFiles)
    assert(TSTypeTest(program.>("h1"), "((number) => number, number) => number"))
    assert(TSTypeTest(program.>("h2"), "(string) => string"))
    assert(TSTypeTest(program.>("h3"), "((number) => number, (number) => number) => (number) => number"))
  }

  test("High Order Declaration Generation") {
    val program = TSProgram(HighOrderFunc.testFiles)
    var writer = DecWriter(HighOrderFunc.diffFile)

    program.visit(writer)
    writer.close
  }
}

object HighOrderFunc {
  private val testFiles = TSTypeTest.tsPathes(Seq("HighOrderFunc.ts"))
  private val diffFile = TSTypeTest.diffPath("HighOrder.d.mls")
}