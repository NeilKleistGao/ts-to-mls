package ts2mls.types;

import scala.collection.mutable.HashMap

abstract class TSType {
  override def toString(): String = ???
  def >(fieldName: String): TSType = throw new java.lang.Exception("Field is not allowed.")
}

case class TSPrimitiveType(typeName: String) extends TSType {
  override def toString(): String = typeName
}

case class TSFunctionType(params: List[TSType], res: TSType) extends TSType {
  override def toString(): String =
    if (params.length == 0) res.toString()
    else if (params.length == 1) s"${params(0).toString()} => ${res.toString()}"
    else {
      val ps = params.foldLeft("")((ls: String, p: TSType) => ls + p.toString() + ", ")
      s"(${ps.substring(0, ps.length() - 2)}) => ${res.toString()}"
    }
}

case class TSClassType(name: String, members: Map[String, TSType]) extends TSType {
  override def toString(): String = s"class $name"

  override def >(fieldName: String): TSType =
    members.getOrElse(fieldName, throw new java.lang.Exception(s"Field \"$fieldName\" not found."))
}