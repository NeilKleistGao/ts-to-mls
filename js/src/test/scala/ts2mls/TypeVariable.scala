package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class TypeVariable extends AnyFunSuite {
  test("Type Variable") {
    val program = TSProgram(TypeVariable.testFiles)

    assert(TSTypeTest(program.>("inc"), "(T') => number where T' <: number"))
    assert(TSTypeTest(program.>("CC"), "class CC where T' <: string"))

    assert(TSTypeTest(program.>("setStringPrinter"), "(Printer<string>) => void"))
    assert(TSTypeTest(program.>("getStringPrinter"), "Printer<string>"))
  }

  test("Type Variable Declaration Generation") {
    val program = TSProgram(TypeVariable.testFiles)
    var writer = DecWriter(TypeVariable.diffFile)

    program.visit(writer)
    writer.close
  }
}

object TypeVariable {
  private val testFiles = TSTypeTest.tsPathes(Seq("TypeVariable.ts"))
  private val diffFile = TSTypeTest.diffPath("TypeVariable.d.mls")
}