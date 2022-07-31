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
  }

  test("Inherit") {
    val program = TSProgram(Seq("src/test/typescript/Inherit.ts"))
    val cls: TSType = program.>("B")
    assert(TypeCompare(cls.>("foo"), "void"))
  }
}
