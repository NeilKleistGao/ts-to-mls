package ts2mls

import org.scalatest.funsuite.AnyFunSuite
import ts2mls.TSProgram
import ts2mls.types._

class Overload extends AnyFunSuite {
  test("Overload") {
    val program = TSProgram(Overload.testFiles)
    assert(TSTypeTest(program.>("f"), "((number) => number) & ((string) => string)"))

    assert(TSTypeTest(program.>("M").>("foo"), "((number) => number) & ((string) => string)"))

    assert(TSTypeTest(program.>("app"), "(((number) => void, number) => void) & (((string) => void, string) => void)"))
    assert(TSTypeTest(program.>("create"), "((number) => number) & ((boolean) => boolean)"))
    assert(TSTypeTest(program.>("g0"), "((string[]) => string) & ((object[]) => object)"))
    assert(TSTypeTest(program.>("db"), "((number) => number[]) & ((object) => object[])"))

    assert(TSTypeTest(program.>("id"), "((M) => {foo: (any) => any}) & ((N) => {})"))

    assert(TSTypeTest(program.>("tst"), "(({z: number}) => {y: string}) & (({z: boolean}) => {y: string})"))
    assert(TSTypeTest(program.>("op"), "((number, number | undefined) => void) & ((number, boolean | undefined) => void)"))
    assert(TSTypeTest(program.>("swap"), "(([number, string]) => [string, number]) & (([string, number]) => [number, string])"))
    assert(TSTypeTest(program.>("u"), "((number | boolean) => string) & ((object) => string | object)"))
    assert(TSTypeTest(program.>("doSome"), "((T' & U') => T' & U') & ((string) => never)"))

    assert(TSTypeTest(program.>("bar"), "((G<string>) => G<string>) & ((G<number>) => G<number>) & ((G<boolean>) => G<boolean>)"))
  }
}

object Overload {
  private val testFiles = TSTypeTest.tsPathes(Seq("Overload.ts"))
}
