import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Tuple extends AnyFunSuite {
  test("Tuple") {
    val program = TSProgram(Seq("src/test/typescript/Tuple.ts"))
    assert(TypeCompare(program.>("key"), "([string, boolean]) => string"))
    assert(TypeCompare(program.>("value"), "([string, boolean]) => boolean"))
    assert(TypeCompare(program.>("third"), "([number, number, number]) => number"))
    assert(TypeCompare(program.>("vec2"), "(number, number) => [number, number]"))
  }

  test("Tuple Convert") {
    import mlscript._

    val program = TSProgram(Seq("src/test/typescript/Tuple.ts"))

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
