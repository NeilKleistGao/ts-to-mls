package ts2mls.types

object Converter {
  private val primitiveName = Map[String, String](
    "boolean" -> "bool",
    "number" -> "number",
    "string" -> "string",
    "any" -> "anything",
    "unknown" -> "anything",
    "void" -> "unit",
    "null" -> "null",
    "undefined" -> "undefined",
    "never" -> "nothing",
    "object" -> "{}"
  )

  def convert(tsType: TSType): String = tsType match {
    case TSNamedType(typeName) => primitiveName.getOrElse(typeName, typeName)
    case TSFunctionType(params, res, constraint) => {
      val func = 
        if (params.length == 0) s"${primitiveName("void")} -> (${convert(res)})"
        else params.foldRight(s"(${convert(res)})")((tst, mlst) => s"(${convert(tst)}) -> (${mlst})")
      func
      // val consList = convertConstrianedList(constraint)
      // if (consList.length == 0) func
      // else Constrained(func, consList)
    }
    case TSUnionType(lhs, rhs) => s"(${convert(lhs)}) | (${convert(rhs)})"
    case TSIntersectionType(lhs, rhs) => s"(${convert(lhs)}) & (${convert(rhs)})"
    // case v: TSTypeVariable => convertTypeVariable(v)
    // case TSTupleType(lst) => convertTuple(lst)
    case TSArrayType(element) => s"MutArray[${convert(element)}]"
    case TSEnumType(_) => "int"
    // case TSMemberType(base, modifier) => convert(base)
    // case TSInterfaceType(_, members, typeVars, parents) => convertRecord(members, typeVars, parents)
    // case TSClassType(_, members, _, typeVars, parents) => convertRecord(members, typeVars, parents)
    // case TSApplicationType(base, applied) => base match {
    //   case TSNamedType(name) => AppliedType(TypeName(name), applied.map((ts) => convert(ts)))
    //   case _ => throw new java.lang.Exception(s"Wrong Base Type in TSApplicationType: $base") // TODO: can we find the name?
    // }
    // case _ => throw new java.lang.Exception("Unknown TypeScript Type")
    case _ => ""
  }

  // private def convertTypeVariable(tstv: TSTypeVariable) = TypeName(tstv.name)
  // private def convertConstrained(tstv: TSTypeVariable): Option[Bounds] = tstv.constraint match {
  //   case Some(v) => Some(Bounds(Bot, convert(v)))
  //   case _ => None
  // }

  // private def convertConstrianedList(typeVars: List[TSTypeVariable]) =
  //   typeVars.foldLeft(List[(TypeVar, Bounds)]())((lst, v) => convertConstrained(v) match {
  //       case Some(bd) => lst :+ (TypeVar(Right(v.name), None) -> bd)
  //       case _ => lst
  //     })

  // private def convertTuple(types: List[TSType]): mlscript.Tuple =
  //   mlscript.Tuple(types.map((t) => None -> convertField(t)))

  // private def convertField(tst: TSType): Field = {
  //   val t = convert(tst)
  //   t match {
  //     case Function(lhs, rhs) => Field(Some(lhs), rhs)
  //     case _ => Field(None, t) 
  //   }
  // }

  // private def convertRecord(members: Map[String, TSMemberType]): Record =
  //   Record(members.toList.foldLeft(List[(Var, Field)]())((list, m) => m._2.modifier match {
  //     case Public => list :+ (Var(m._1), convertField(m._2))
  //     case _ => list
  //   }))

  // private def convertRecord(members: Map[String, TSMemberType], typeVars: List[TSTypeVariable], parents: List[TSType]): Type = {
  //   val rec: Record = convertRecord(members)
  //   val cons = convertConstrianedList(typeVars)
  //   val typedInt = if (cons.isEmpty) rec else Constrained(rec, cons)
    
  //   parents.foldLeft(typedInt)((t, p) => convert(p) match {
  //     case r: Record => WithExtension(t, r)
  //     case _ => t
  //   })
  // }
}