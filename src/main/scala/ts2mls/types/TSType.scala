package ts2mls.types;

abstract class TSType {
  override def toString(): String = ???
}

case class TSPrimitiveType(typeName: String) extends TSType {
  override def toString(): String = typeName
}

case class TSFunctionType(params: List[TSType], res: TSType) extends TSType {
  override def toString(): String =
    if (params.length == 0) res.toString()
    else {
      val ps = params.foldLeft("")((ls: String, p: TSType) => ls + p.toString() + ", ")
      s"(${ps.substring(0, ps.length() - 2)}) => ${res.toString()}"
    }
}