import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Namespace extends AnyFunSuite {
  test("Namespace") {
    val program = TSProgram(Namespace.testFiles)
    val ns = program.>>("N1")
    assert(TSTypeTest(ns.>("f"), "(any) => number"))
    assert(TSTypeTest(ns.>("C").>("f"), "void"))
    assert(TSTypeTest(ns.>>("N2").>("fff"), "(boolean) => number"))
    assert(TSTypeTest(ns.>>("N2").>("f"), "(any) => number"))
  }
}

object Namespace {
  private val testFiles = TSTypeTest.tsPathes(Seq("NameSpace.ts"))
}