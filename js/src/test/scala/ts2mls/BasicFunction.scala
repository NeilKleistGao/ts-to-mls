package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class BasicFunction extends AnyFunSuite {
  test("Basic Function") {
    val program = TSProgram(BasicFunction.testFiles)
    assert(TSTypeTest(program.>("hello"), "void"))
    assert(TSTypeTest(program.>("add"), "(number, number) => number"))
    assert(TSTypeTest(program.>("sub"), "(number, number) => number"))
    assert(TSTypeTest(program.>("foo"), "number"))
    assert(TSTypeTest(program.>("id"), "(any) => any"))
    assert(TSTypeTest(program.>("odd"), "(number) => boolean"))
    assert(TSTypeTest(program.>("isnull"), "(any) => boolean"))
    assert(TSTypeTest(program.>("bar"), "any"))
    assert(TSTypeTest(program.>("nu"), "(null) => null"))
    assert(TSTypeTest(program.>("un"), "(undefined) => undefined"))
    assert(TSTypeTest(program.>("fail"), "never"))
    assert(TSTypeTest(program.>("create"), "object"))
    assert(TSTypeTest(program.>("pa"), "(number) => number"))
    assert(TSTypeTest(program.>("wtf"), "(unknown) => void"))
  }

  test("Basic Function Declaration Generation") {
    val program = TSProgram(BasicFunction.testFiles)
    var writer = DecWriter(BasicFunction.diffFile)

    writer.output("hello", program.getMLSType("hello").show)
    writer.output("add", program.getMLSType("add").show)
    writer.output("sub", program.getMLSType("sub").show)
    writer.output("foo", program.getMLSType("foo").show)
    writer.output("id", program.getMLSType("id").show)
    writer.output("odd", program.getMLSType("odd").show)
    writer.output("isnull", program.getMLSType("isnull").show)
    writer.output("bar", program.getMLSType("bar").show)
    writer.output("nu", program.getMLSType("nu").show)
    writer.output("un", program.getMLSType("un").show)
    writer.output("fail", program.getMLSType("fail").show)
    writer.output("create", program.getMLSType("create").show)
    writer.output("pa", program.getMLSType("pa").show)
    writer.output("wtf", program.getMLSType("wtf").show)

    writer.close
  }
}

object BasicFunction {
  private val testFiles = TSTypeTest.tsPathes(Seq("BasicFunctions.ts"))
  private val diffFile = TSTypeTest.diffPath("BasicFunctions.d.mls")
}