package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class UnionTest extends AnyFunSuite {
  test("Union") {
    val program = TSProgram(UnionTest.testFiles)
    assert(TSTypeTest(program.>("getString"), "(string | number | boolean) => string"))
    assert(TSTypeTest(program.>("test"), "(boolean) => string | number"))
    assert(TSTypeTest(program.>("run"), "(((number) => number) | ((number) => string)) => any"))
    assert(TSTypeTest(program.>("get"), "(number[] | string[]) => void"))
    assert(TSTypeTest(program.>("get2"), "([string, string] | [number, string]) => string"))
    assert(TSTypeTest(program.>("typeVar"), "(T' | U') => T' | U'"))
  }

  test("Union Convert") {
    import mlscript._

    val program = TSProgram(UnionTest.testFiles)

    program.getMLSType("getString") match {
      case Function(lhs, rhs) => {
        lhs match {
          case Union(lhs2, rhs2) => {
            lhs2 match {
              case Union(lhs3, rhs3) => lhs3 match {
                case TypeName(name) => assert(name.equals("string"))
                case _ => assert(false)
              }
              case _ => assert(false)
            }
          }
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }
  }
}

object UnionTest {
  private val testFiles = TSTypeTest.tsPathes(Seq("Union.ts"))
}