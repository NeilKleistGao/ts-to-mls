package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class TupleTest extends AnyFunSuite {
  test("Tuple") {
    val program = TSProgram(TupleTest.testFiles)
    assert(TSTypeTest(program.>("key"), "([string, boolean]) => string"))
    assert(TSTypeTest(program.>("value"), "([string, boolean]) => boolean"))
    assert(TSTypeTest(program.>("third"), "([number, number, number]) => number"))
    assert(TSTypeTest(program.>("vec2"), "(number, number) => [number, number]"))
    assert(TSTypeTest(program.>("twoFunctions"), "([(number) => number, (number) => number], number) => number"))
    assert(TSTypeTest(program.>("tupleIt"), "(string) => [string]"))
    assert(TSTypeTest(program.>("s"), "(boolean) => [string | number, number | false | true]"))
    assert(TSTypeTest(program.>("s2"), "([boolean, string | number]) => string | number"))
    assert(TSTypeTest(program.>("ex"), "(T', U') => [T', U', T' & U']"))
    assert(TSTypeTest(program.>("foo"), "([T' & U']) => void"))
    assert(TSTypeTest(program.>("conv"), "({y: number}) => [{y: number}, {z: string}]"))
    assert(TSTypeTest(program.>("swap"), "([{x: number}, {}]) => [B, A]"))
    assert(TSTypeTest(program.>("fff"), "(FFF<string>, string) => void"))
    assert(TSTypeTest(program.>("getFFF"), "FFF<number>"))
  }

  test("Tuple Convert") {
    import mlscript._

    val program = TSProgram(TupleTest.testFiles)

    program.getMLSType("key") match {
      case Function(p, r) => p match {
        case mlscript.Tuple(lst) => {
          lst(0) match {
            case None -> Field(in, out) => {
              assert(in.isEmpty)
              out match {
                case TypeName(name) => assert(name.equals("string"))
                case _ => assert(false)
              }
            }
          }

          lst(1) match {
            case None -> Field(in, out) => {
              assert(in.isEmpty)
              out match {
                case TypeName(name) => assert(name.equals("bool"))
                case _ => assert(false)
              }
            }
          }
        }
        case _ => assert(false)
      } 
      case _ => assert(false)
    }
  }
}

object TupleTest {
  private val testFiles = TSTypeTest.tsPathes(Seq("Tuple.ts"))
}