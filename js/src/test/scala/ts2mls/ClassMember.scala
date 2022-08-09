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

  test("Class Declaration Generation") {
    val program = TSProgram(ClassMember.classMemberFiles)
    var writer = DecWriter(ClassMember.classMemberDiff)

    program.visit(writer)
    writer.close
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

  test("Inherit Declaration Generation") {
    val program = TSProgram(ClassMember.inheritFiles)
    var writer = DecWriter(ClassMember.inheritDiff)

    program.visit(writer)
    writer.close
  }
}

object ClassMember {
  private val classMemberFiles = TSTypeTest.tsPathes(Seq("ClassMember.ts"))
  private val inheritFiles = TSTypeTest.tsPathes(Seq("Inherit.ts"))
  private val classMemberDiff = TSTypeTest.diffPath("ClassMember.d.mls")
  private val inheritDiff = TSTypeTest.diffPath("Inherit.d.mls")
}