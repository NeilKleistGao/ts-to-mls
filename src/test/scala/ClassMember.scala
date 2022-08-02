import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class ClassMember extends AnyFunSuite {
  test("Class Member") {
    val program = TSProgram(Seq("src/test/typescript/ClassMember.ts"))
    val cls: TSType = program.>("Student")
    assert(TypeCompare(cls.>("getID"), "number"))
    assert(TypeCompare(cls.>("addScore"), "(string, number) => void"))
    assert(TypeCompare(cls.>("isFriend"), "(Student) => boolean"))

    assert(TypeCompare(program.>("Foo").>("bar"), "(T') => void"))
  }

  test("Inherit") {
    val program = TSProgram(Seq("src/test/typescript/Inherit.ts"))
    val cls: TSType = program.>("B")
    assert(TypeCompare(cls.>("foo"), "void"))

    program.>("D") match {
      case TSClassType(_, _, _, parents) => assert(TypeCompare(parents(0), "class C<number>"))
    }

    assert(TypeCompare(program.>("D").>("set"), "(number) => void"))
    assert(TypeCompare(program.>("D").>("get"), "number"))
  }
}
