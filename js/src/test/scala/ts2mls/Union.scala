package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Union extends AnyFunSuite {
  test("Union") {
    val program = TSProgram(Union.testFiles)
    assert(TSTypeTest(program.>("getString"), "(string | number | boolean) => string"))
    assert(TSTypeTest(program.>("test"), "(boolean) => string | number"))
    assert(TSTypeTest(program.>("run"), "(((number) => number) | ((number) => string)) => any"))
    assert(TSTypeTest(program.>("get"), "(number[] | string[]) => void"))
    assert(TSTypeTest(program.>("get2"), "([string, string] | [number, string]) => string"))
    assert(TSTypeTest(program.>("typeVar"), "(T' | U') => T' | U'"))
  }

  test("Union Declaration Generation") {
    val program = TSProgram(Union.testFiles)
    var writer = DecWriter(Union.diffFile)

    program.visit(writer)
    writer.close
  }
}

object Union {
  private val testFiles = TSTypeTest.tsPathes(Seq("Union.ts"))
  private val diffFile = TSTypeTest.diffPath("Union.d.mls")
}