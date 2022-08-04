import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Enum extends AnyFunSuite {
  test("Enum") {
    val program = TSProgram(Seq("src/test/typescript/Enum.ts"))
    assert(TypeCompare(program.>("pass"), "(Color) => boolean"))
    assert(TypeCompare(program.>("stop"), "Color"))
  }

  test("Enum Convert") {
    import mlscript._

    val program = TSProgram(Seq("src/test/typescript/Enum.ts"))

    program.getMLSType("stop") match {
      case Function(p, r) => r match {
        case TypeName(name) => assert(name.equals("int"))
        case _ => assert(false)
      }
      case _ => assert(false)
    }
  }
}
