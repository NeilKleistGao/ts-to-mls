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

  test("Array Convert") {
    import mlscript._

    val program = TSProgram(Array.testFiles)

    program.getMLSType("getZero3") match {
      case Function(p, r) => r match {
        case TypeName(name) => assert(name.equals("MutArray"))
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }
}

object Array {
  private val testFiles = TSTypeTest.tsPathes(Seq("Array.ts"))
}