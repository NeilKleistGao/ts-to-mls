package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Intersection extends AnyFunSuite {
  test("Intersection") {
    val program = TSProgram(Intersection.testFiles)
    assert(TSTypeTest(program.>("extend"), "(T', U') => T' & U'"))
    assert(TSTypeTest(program.>("foo"), "(T' & U') => void"))
  }

  test("Intersection Declaration Generation") {
    val program = TSProgram(Intersection.testFiles)
    var writer = DecWriter(Intersection.diffFile)

    program.visit(writer)
    writer.close
  }
}

object Intersection {
  private val testFiles = TSTypeTest.tsPathes(Seq("Intersection.ts"))
  private val diffFile = TSTypeTest.diffPath("Intersection.d.mls")
}