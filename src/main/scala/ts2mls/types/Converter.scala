package ts2mls.types

import mlscript._

object Converter {
  private val primitiveName = Map[String, Type](
    "boolean" -> new TypeName("bool"),
    "number" -> new TypeName("number"),
    "string" -> new TypeName("string"),
    "any" -> Top,
    "void" -> new TypeName("unit"),
    "null" -> new TypeName("null"),
    "undefined" -> new TypeName("undefined"),
    "never" -> Bot,
    "object" -> new Record(List())
  )

  def convert(tsType: TSType): Type = tsType match {
    case TSNamedType(typeName) => primitiveName(typeName)
    case TSFunctionType(params, res, constraint) =>
      if (params.length == 0) new Function(primitiveName("void"), convert(res))
      else params.foldRight[Type](convert(res))((tst, mlst) => new Function(convert(tst), mlst))
    case _ => new TypeName("") // TODO:
  }
}