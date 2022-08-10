package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class MultiFiles extends AnyFunSuite {
  test("Multiple Files Declaration Generation") {
    val program = TSProgram(MultiFiles.testFiles)
    var writer = DecWriter(MultiFiles.diffFile)

    program.visit(writer)
    writer.close
  }
}

object MultiFiles {
  private val testFiles = TSTypeTest.tsPathes(Seq("Multi1.ts", "Multi2.ts"))
  private val diffFile = TSTypeTest.diffPath("MultiFiles.d.mls")
}