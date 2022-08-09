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

  test("Intersection Convert") {
    import mlscript._

    val program = TSProgram(Intersection.testFiles)
    program.getMLSType("foo") match {
      case Function(p, r) => p match {
        case Inter(lhs, rhs) => {
          lhs match {
            case TypeName(name) => assert(name.equals("T"))
            case _ => assert(false)
          }

          rhs match {
            case TypeName(name) => assert(name.equals("U"))
            case _ => assert(false)
          }
        }
      }
      case _ => assert(false)
    }
  }
}

object Intersection {
  private val testFiles = TSTypeTest.tsPathes(Seq("Intersection.ts"))
}