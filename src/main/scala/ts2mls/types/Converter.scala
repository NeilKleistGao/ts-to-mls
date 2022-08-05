package ts2mls.types

import mlscript._

object Converter {
  private val primitiveName = Map[String, Type](
    "boolean" -> TypeName("bool"),
    "number" -> TypeName("number"),
    "string" -> TypeName("string"),
    "any" -> Top,
    "unknown" -> Top,
    "void" -> TypeName("unit"),
    "null" -> TypeName("null"),
    "undefined" -> TypeName("undefined"),
    "never" -> Bot,
    "object" -> Record(List())
  )

  def convert(tsType: TSType): Type = tsType match {
    case TSNamedType(typeName) => primitiveName(typeName)
    case TSFunctionType(params, res, constraint) => {
      val func = 
        if (params.length == 0) Function(primitiveName("void"), convert(res))
        else params.foldRight[Type](convert(res))((tst, mlst) => Function(convert(tst), mlst))
      val consList = constraint.foldLeft(List[(TypeVar, Bounds)]())((lst, v) => convertConstrained(v) match {
        case Some(bd) => lst :+ (TypeVar(Right(v.name), None) -> bd)
        case _ => lst
      })

      if (consList.length == 0) func
      else Constrained(func, consList)
    }
    case TSUnionType(lhs, rhs) => Union(convert(lhs), convert(rhs))
    case TSIntersectionType(lhs, rhs) => Inter(convert(lhs), convert(rhs))
    case v: TSTypeVariable => convertTypeVariable(v)
    case TSTupleType(lst) => convertTuple(lst)
    case TSArrayType(_) => TypeName("MutArray")
    case TSEnumType(_) => TypeName("int")
    case TSInterfaceType(_, members, typeVars, parents) => {
      val int = convertRecord(members)
      int
    }
    case _ => throw new java.lang.Exception("Unknown TypeScript Type") // TODO: more types support
  }

  private def convertTypeVariable(tstv: TSTypeVariable) = TypeName(tstv.name)
  private def convertConstrained(tstv: TSTypeVariable): Option[Bounds] = tstv.constraint match {
    case Some(v) => Some(Bounds(Bot, convert(v)))
    case _ => None
  }

  private def convertTuple(types: List[TSType]): mlscript.Tuple =
    mlscript.Tuple(types.map((t) => None -> convertField(t)))

  private def convertField(tst: TSType): Field = {
    val t = convert(tst)
    t match {
      case Function(lhs, rhs) => Field(Some(lhs), rhs)
      case _ => Field(None, t) 
    }
  }

  private def convertRecord(members: Map[String, TSType]) =
    Record(members.toList.foldLeft(List[(Var, Field)]())((list, m) => {
      list :+ (Var(m._1), convertField(m._2))
    }))
}