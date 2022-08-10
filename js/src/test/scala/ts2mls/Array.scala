package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Array extends AnyFunSuite {
  test("Array") {
    val program = TSProgram(Array.testFiles)
    assert(TSTypeTest(program.>("first"), "(string[]) => string"))
    assert(TSTypeTest(program.>("getZero3"), "number[]"))
    assert(TSTypeTest(program.>("first2"), "(((number) => number)[]) => (number) => number"))
  }

  test("Array Declaration Generation") {
    val program = TSProgram(Array.testFiles)
    var writer = DecWriter(Array.diffFile)

    program.visit(writer)
    writer.close
  }
}

object Array {
  private val testFiles = TSTypeTest.tsPathes(Seq("Array.ts"))
  private val diffFile = TSTypeTest.diffPath("Array.d.mls")
}