package ts2mls.types;

import scala.collection.mutable.HashMap

abstract class TSType {
  override def toString(): String = ???
  def >(fieldName: String): TSType = throw new java.lang.Exception("Field is not allowed.")
}

case class TSNamedType(typeName: String) extends TSType {
  override def toString(): String = typeName
}

case class TSTupleType(types: List[TSType]) extends TSType {
  override def toString(): String = {
    val body = types.foldLeft("")((s, t) => s"$s$t, ")
    s"[${body.substring(0, body.length() - 2)}]"
  }
}

case class TSFunctionType(params: List[TSType], res: TSType, constraint: Map[String, TSType]) extends TSType {
  override def toString(): String = {
    val rhs = res match {
      case TSFunctionType(rp, _, _) if (params.length > 0 && rp.length > 0) => s"(${res.toString()})"
      case u: TSUnionType => s"(${u.toString()})"
      case _ => res.toString()
    }
    val body = 
      if (params.length == 0) rhs
      else if (params.length == 1) {
        params(0) match {
          case ut: TSUnionType => s"(${params(0).toString()}) => ${rhs}"
          case _ => s"${params(0).toString()} => ${rhs}"
        }
      } 
      else {
        val ps = params.foldLeft("")((ls: String, p: TSType) => ls + p.toString() + ", ")
        s"(${ps.substring(0, ps.length() - 2)}) => ${rhs}"
      }

    if (constraint.isEmpty) body
    else {
      val cons = constraint.foldLeft("")((s, p) => s"$s${p._1} <: ${p._2}, ")
      s"$body where ${cons.substring(0, cons.length() - 2)}"
    }
  }
}

case class TSClassType(name: String, members: Map[String, TSType], constraint: Map[String, TSType], parents: List[TSType])
  extends TSFieldType(members, parents) {
  override def toString(): String = if (constraint.isEmpty) s"class $name"
    else {
      val cons = constraint.foldLeft("")((s, p) => s"$s${p._1} <: ${p._2}, ")
      s"class $name where ${cons.substring(0, cons.length() - 2)}"
    }
}

case class TSInterfaceType(name: String, members: Map[String, TSType], constraint: Map[String, TSType], parents: List[TSType])
  extends TSFieldType(members, parents) {
  override def toString(): String = {
    val memString = members.foldLeft("")((str, it) => str + s"\n\t${it._1}: ${it._2.toString()}")
    val body = s"interface $name {$memString\n}"

    if (constraint.isEmpty) body
    else {
      val cons = constraint.foldLeft("")((s, p) => s"$s${p._1} <: ${p._2}, ")
      s"$body where ${cons.substring(0, cons.length() - 2)}"
    }
  }
}

case class TSArrayType(eleType: TSType) extends TSType {
  override def toString(): String = eleType match {
    case t: TSNamedType => s"$t[]"
    case t: TSFunctionType => s"($t)[]"
    case t: TSUnionType => s"($t)[]"
  }
}

case class TSUnionType(lhs: TSType, rhs: TSType) extends TSType {
  override def toString(): String = {
    val ls = lhs match {
      case f: TSFunctionType => s"($f)"
      case t: TSType => t.toString
    }

    val rs = rhs match {
      case f: TSFunctionType => s"($f)"
      case t: TSType => t.toString
    }

    s"$ls | $rs"
  }
}