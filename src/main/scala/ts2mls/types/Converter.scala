package ts2mls.types

import mlscript._

object Converter {
  def convert(tsType: TSType): Type = tsType match {
    case TSNamedType(typeName) => new TypeName(typeName)
    case TSFunctionType(params, res, constraint) => 
      new Function(params.foldLeft[Type](Bot)((mlst, tst) => mlst match {
        case Bot => convert(tst)
        case _ => new Function(mlst, convert(tst))
      }), convert(res))
    case _ => new TypeName("") // TODO:
  }
}