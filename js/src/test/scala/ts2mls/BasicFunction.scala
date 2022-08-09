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

  test("Basic Convert") {
    import mlscript._

    val program = TSProgram(BasicFunction.testFiles)

    program.getMLSType("hello") match {
      case Function(lhs, rhs) => {
        rhs match {
          case TypeName(name) => assert(name.equals("unit"))
          case _ => assert(false)
        }

        lhs match {
          case TypeName(name) => assert(name.equals("unit"))
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }

    program.getMLSType("add") match {
      case Function(lhs, rhs) => {
        lhs match {
          case TypeName(name) => assert(name.equals("number"))
          case _ => assert(false)
        }

        rhs match {
          case Function(lhs2, rhs2) => assert(true)
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }

    program.getMLSType("id") match {
      case Function(lhs, rhs) => {
        lhs match {
          case Top => assert(true)
          case _ => assert(false)
        }

        rhs match {
          case Top => assert(true)
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }

    program.getMLSType("wtf") match {
      case Function(p, r) => p match {
        case Top => assert(true)
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  } 
}

object BasicFunction {
  private val testFiles = TSTypeTest.tsPathes(Seq("BasicFunctions.ts"))
}