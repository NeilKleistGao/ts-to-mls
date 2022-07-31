import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Union extends AnyFunSuite {
  test("Union") {
    val program = TSProgram(Seq("src/test/typescript/Union.ts"))
    assert(TypeCompare(program.>("getString"), "(string | number | boolean) => string"))
    assert(TypeCompare(program.>("test"), "boolean => (string | number)"))
    assert(TypeCompare(program.>("run"), "((number => number) | (number => string)) => any"))
    assert(TypeCompare(program.>("get"), "(number[] | string[]) => void"))
    assert(TypeCompare(program.>("get2"), "([string, string] | [number, string]) => string"))
  }

  test("Union Convert") {
    import mlscript._

    val program = TSProgram(Seq("src/test/typescript/Union.ts"))

    program.getMLSType("test") match {
      case Function(lhs, rhs) => {
        rhs match {
          case Union(lhs, rhs) => {
            lhs match {
              case TypeName(name) if (name.equals("string")) => assert(true)
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
