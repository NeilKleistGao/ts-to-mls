package ts2mls.types

import mlscript._

object Converter {
  private val primitiveName = Map[String, Type](
    "number" -> new TypeName("int"),
    "string" -> new TypeName("string"),
    "boolean" -> new TypeName("bool"),
    "void" -> Bot,
    "any" -> Top
  )

  def convert(tsType: TSType): Type = tsType match {
    case TSNamedType(typeName) => primitiveName(typeName)
    case TSFunctionType(params, res, constraint) =>
      if (params.length == 0) new Function(Bot, convert(res))
      else params.foldRight[Type](convert(res))((tst, mlst) => new Function(convert(tst), mlst))
    case _ => new TypeName("") // TODO:
  }
}