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

  test("Interface Convert") {
    import mlscript._

    val program = TSProgram(InterfaceMember.testFiles)

    program.getMLSType("Simple") match {
      case Record(fields) => {
        fields(0)._1 match {
          case Var(name) => assert(name.equals("a"))
          case _ => assert(false)
        }

        fields(0)._2 match {
          case Field(in, out) => out match {
            case TypeName(name) => assert(name.equals("number"))
            case _ => assert(false)
          } 
          case _ => assert(false)
        }

        fields(1)._1 match {
          case Var(name) => assert(name.equals("b"))
          case _ => assert(false)
        }

        fields(1)._2 match {
          case Field(in, out) => {
            in match {
              case Some(p) => p match {
                case TypeName(name) => assert(name.equals("bool"))
                case _ => assert(false)
              }
              case _ => assert(false)
            }

            out match {
              case TypeName(name) => assert(name.equals("string"))
              case _ => assert(false)
            } 
          }
          case _ => assert(false)
        }
      } 
      case _ => assert(false)
    }

    program.getMLSType("Simple2") match {
      case Constrained(base, where) => {
        base match {
          case Record(members) => members(0)._2 match {
            case Field(in, out) => out match {
              case TypeName(name) => assert(name.equals("T"))
              case _ => assert(false)
            }
            case _ => assert(false)
          }
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }

    program.getMLSType("Next") match {
      case WithExtension(base, record) => {
        base match {
          case r: Record => assert(true)
          case _ => assert(false)
        }

        record match {
          case Record(m) => m(0)._1 match {
            case Var(name) => assert(name.equals("a"))
            case _ => assert(false)
          }
          case _ => assert(false)
        }
      }
      case _ => assert(false)
    }
  }
}

object InterfaceMember {
  private val testFiles = TSTypeTest.tsPathes(Seq("InterfaceMember.ts"))
}