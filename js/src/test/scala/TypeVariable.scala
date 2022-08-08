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

  test("Type Variable Convert") {
    import mlscript._

    val program = TSProgram(TypeVariable.testFiles)

    program.getMLSType("inc") match {
      case Constrained(base, list) => {
        base match {
          case Function(p, r) => p match {
            case TypeName(name) => assert(name.equals("T"))
            case _ => assert(false)
          }
          case _ => assert(false)
        }

        assert(list.length == 1)
        list(0)._1 match {
          case v: TypeVar => assert(v.toString().equals("T"))
          case _ => assert(false)
        }

        list(0)._2 match {
          case Bounds(lb, ub) => ub match {
            case TypeName(name) => assert(name.equals("number"))
            case _ => assert(false)
          }
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }

    program.getMLSType("getStringPrinter") match {
      case Function(p, r) => r match {
        case AppliedType(base, applied) => {
          base match {
            case TypeName(name) => assert(name.equals("Printer"))
          }

          assert(applied.length == 1)
          applied(0) match {
            case TypeName(name) => assert(name.equals("string"))
            case _ => assert(false)
          }
        }
        case _ => assert(false)
      } 
      case _ => assert(false)
    }
  }
}

object TypeVariable {
  private val testFiles = TSTypeTest.tsPathes(Seq("TypeVariable.ts"))
}