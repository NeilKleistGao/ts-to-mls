package mlscript

import mlscript.utils._, shorthands._

// from https://github.com/hkust-taco/mlscript/blob/mlscript/shared/src/main/scala/mlscript/syntax.scala
// only for the test
// Types

sealed abstract class Type

sealed abstract class Composed(val pol: Boolean) extends Type

final case class Union(lhs: Type, rhs: Type)             extends Composed(true)
final case class Inter(lhs: Type, rhs: Type)             extends Composed(false)
final case class Function(lhs: Type, rhs: Type)          extends Type
final case class Record(fields: List[Var -> Field])        extends Type
final case class Tuple(fields: List[Option[Var] -> Field])    extends Type
final case class Recursive(uv: TypeVar, body: Type)      extends Type
final case class AppliedType(base: TypeName, targs: List[Type]) extends Type
final case class Neg(base: Type)                         extends Type
final case class Rem(base: Type, names: List[Var])         extends Type
final case class Bounds(lb: Type, ub: Type)              extends Type
final case class WithExtension(base: Type, rcd: Record)  extends Type
final case class Constrained(base: Type, where: List[TypeVar -> Bounds]) extends Type

final case class Field(in: Option[Type], out: Type)

sealed abstract class NullaryType                        extends Type

case object Top                                          extends NullaryType
case object Bot                                          extends NullaryType

/** Literal type type, e.g. type `0` is a type with only one possible value `0`. */
final case class Literal(lit: Lit)                       extends NullaryType

/** Reference to an existing type with the given name. */
final case class TypeName(name: String)                     extends NullaryType

final case class TypeVar(val identifier: Int \/ String, nameHint: Option[String]) extends NullaryType {
  // ^ The better data structure to represent this would be an EitherOrBoth
  override def toString: String = identifier.fold("α" + _, identity)
}

final case class PolyType(targs: List[TypeName], body: Type)

sealed abstract class Lit
case class Var(name: String)

final case class IntLit(value: BigInt)            extends Lit
final case class DecLit(value: BigDecimal)        extends Lit
final case class StrLit(value: String)            extends Lit
final case class UnitLit(undefinedOrNull: Boolean)   extends Lit