package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class InterfaceMember extends AnyFunSuite {
  test("Interface Member") {
    val program = TSProgram(InterfaceMember.testFiles)
    assert(TSTypeTest(program.>("IFoo"), "interface IFoo {\n\ta: string\n\tb: (number) => number\n\tc: boolean\n\td: (string) => void\n}"))
    assert(TSTypeTest(program.>("IFoo").>("a"), "string"))
    assert(TSTypeTest(program.>("IFoo").>("b"), "(number) => number"))
    assert(TSTypeTest(program.>("IFoo").>("c"), "boolean"))
    assert(TSTypeTest(program.>("IFoo").>("d"), "(string) => void"))

    assert(TSTypeTest(program.>("II").>("test"), "(T') => number"))

    // Should we consider it as an optional field?
    assert(TSTypeTest(program.>("create"), "{v: number | undefined}"))
    assert(TSTypeTest(program.>("get"), "({t: string}) => string"))

    assert(TSTypeTest(program.>("IEvent").>("callback"), "(number) => void"))

    assert(TSTypeTest(program.>("SearchFunc").>("__call"), "(string, string) => boolean"))
    assert(TSTypeTest(program.>("StringArray").>("__index"), "(number) => string"))
    assert(TSTypeTest(program.>("Counter").>("__call"), "(number) => string"))
    assert(TSTypeTest(program.>("Counter").>("interval"), "number"))
    assert(TSTypeTest(program.>("Counter").>("reset"), "void"))
  }

  test("Interface Function Declaration Generation") {
    val program = TSProgram(InterfaceMember.testFiles)
    var writer = DecWriter(InterfaceMember.diffFile)

    program.visit(writer)
    writer.close
  }
}

object InterfaceMember {
  private val testFiles = TSTypeTest.tsPathes(Seq("InterfaceMember.ts"))
  private val diffFile = TSTypeTest.diffPath("InterfaceMember.d.mls")
}