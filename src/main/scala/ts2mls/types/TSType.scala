package ts2mls.types;

import scala.collection.mutable.HashMap

abstract class TSType {
  val priority: Int = 0
  override def toString(): String = ???
  def >(fieldName: String): TSType = throw new java.lang.Exception("Field is not allowed.")
}

case class TSTypeVariable(val name: String, constraint: Option[TSType]) extends TSType {
  override val priority = 0

  override def toString(): String = s"$name'"

  def getConstraint(): String = constraint match {
    case Some(t) => s"$name' <: $t"
    case _ => ""
  }
}

case class TSNamedType(typeName: String) extends TSType {
  override val priority = 0

  override def toString(): String = typeName
}

case class TSTupleType(types: List[TSType]) extends TSType {
  override val priority = 0
  override def toString(): String = {
    val body = types.foldLeft("")((s, t) => s"$s$t, ")
    s"[${body.substring(0, body.length() - 2)}]"
  }
}

case class TSFunctionType(params: List[TSType], res: TSType, typeVars: List[TSTypeVariable]) extends TSType {
  override val priority = 2
  override def toString(): String = {
    val rhs = if (res.priority < priority && res.priority > 0) s"($res)" else res.toString()
    val body = 
      if (params.length == 0) res.toString()
      else {
        val ps = params.foldLeft("")((ls: String, p: TSType) => ls + p.toString() + ", ")
        s"(${ps.substring(0, ps.length() - 2)}) => ${rhs}"
      }

    val cons = typeVars.foldLeft("")((s, v) => {
      val c = v.getConstraint()
      if (c.isEmpty()) s else s"$s$c, "
    })

    if (cons.isEmpty()) body
    else s"$body where ${cons.substring(0, cons.length() - 2)}"
  }
}

case class TSClassType(name: String, members: Map[String, TSType], typeVars: List[TSTypeVariable], parents: List[TSType])
  extends TSFieldType(members, parents) {
  override val priority = 0

  override def toString(): String = {
    val body = s"class $name"
    val cons = typeVars.foldLeft("")((s, v) => {
      val c = v.getConstraint()
      if (c.isEmpty()) s else s"$s$c, "
    })

    if (cons.isEmpty()) body
    else s"$body where ${cons.substring(0, cons.length() - 2)}"
  }
}

case class TSInterfaceType(name: String, members: Map[String, TSType], typeVars: List[TSTypeVariable], parents: List[TSType])
  extends TSFieldType(members, parents) {
  override val priority = 0

  override def toString(): String = {
    val memString = members.foldLeft("")((str, it) => str + s"\n\t${it._1}: ${it._2.toString()}")
    val body = s"interface $name {$memString\n}"

    val cons = typeVars.foldLeft("")((s, v) => {
      val c = v.getConstraint()
      if (c.isEmpty()) s else s"$s$c, "
    })

    if (cons.isEmpty()) body
    else s"$body where ${cons.substring(0, cons.length() - 2)}"
  }
}

case class TSArrayType(eleType: TSType) extends TSType {
  override val priority = 3
  override def toString(): String = 
    if (eleType.priority < priority && eleType.priority > 0) s"($eleType)[]"
    else s"$eleType[]"
}

abstract class TSStructuralType(lhs: TSType, rhs: TSType, notion: String) extends TSType {
  override val priority = 1

  override def toString(): String = s"$lhs $notion ${if (rhs.priority == priority) s"($rhs)" else s"$rhs"}"
}

case class TSUnionType(lhs: TSType, rhs: TSType) extends TSStructuralType(lhs, rhs, "|")
case class TSIntersectionType(lhs: TSType, rhs: TSType) extends TSStructuralType(lhs, rhs, "&")