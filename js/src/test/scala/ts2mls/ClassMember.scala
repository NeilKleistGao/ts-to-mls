package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class ClassMember extends AnyFunSuite {
  test("Class Member") {
    val program = TSProgram(ClassMember.classMemberFiles)
    val cls: TSType = program.>("Student")
    assert(TSTypeTest(cls.>("getID"), "number"))
    assert(TSTypeTest(cls.>("addScore"), "(string, number) => void"))
    assert(TSTypeTest(cls.>("isFriend"), "(Student) => boolean"))
    assert(TSTypeTest(cls.>("name"), "string"))
    assert(TSTypeTest(cls.>("a"), "- number"))
    assert(TSTypeTest(cls.>("b"), "o string"))

    assert(TSTypeTest(program.>("Foo").>("bar"), "(T') => void"))
  }

  test("Inherit") {
    val program = TSProgram(ClassMember.inheritFiles)
    val cls: TSType = program.>("B")
    assert(TSTypeTest(cls.>("foo"), "void"))

    program.>("D") match {
      case TSClassType(_, _, _, _, parents) => assert(TSTypeTest(parents(0), "class C<number>"))
    }

    assert(TSTypeTest(program.>("D").>("set"), "(number) => void"))
    assert(TSTypeTest(program.>("D").>("get"), "number"))
  }

  test("Class Convert") {
    import mlscript._

    val program = TSProgram(ClassMember.classMemberFiles)

    program.getMLSType("EZ") match {
      case Record(members) => {
        assert(members.length == 1)

        members(0)._1 match {
          case Var(name) => assert(name.equals("inc"))
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }
  }

  test("Static Members") {
    val program = TSProgram(ClassMember.classMemberFiles)

    program.>("Outer") match {
      case cls: TSClassType => {
        val inner = cls.>>("Inner").base
        assert(TSTypeTest(inner, "class Inner"))
        assert(TSTypeTest(inner.>("a"), "number"))
      }
      case _ => assert(false)
    }
  }
}

object ClassMember {
  private val classMemberFiles = TSTypeTest.tsPathes(Seq("ClassMember.ts"))
  private val inheritFiles = TSTypeTest.tsPathes(Seq("Inherit.ts"))
}