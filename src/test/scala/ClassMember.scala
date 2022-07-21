import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class ClassMember extends AnyFunSuite {
  test("Class Member") {
    val program = TSProgram("src/test/typescript/ClassMember.ts")
    val cls: TSType = program.getType("Student")
    assert(TypeCompare(cls, "class Student"))
    assert(TypeCompare(cls.>("getID"), "number"))
    assert(TypeCompare(cls.>("addScore"), "(string, number) => void"))
    assert(TypeCompare(cls.>("isFriend"), "Student => boolean"))

    try {
        cls.>("name")
        assert(false)
    }
    catch {
        case e: java.lang.Exception => assert(e.getMessage().equals("Field \"name\" not found."))
    }

    try {
        cls.>("__constructor")
        assert(false)
    }
    catch {
        case e: java.lang.Exception => assert(e.getMessage().equals("Field \"__constructor\" not found."))
    }

    try {
        cls.>("constructor")
        assert(false)
    }
    catch {
        case e: java.lang.Exception => assert(e.getMessage().equals("Field \"constructor\" not found."))
    }
  }
}
