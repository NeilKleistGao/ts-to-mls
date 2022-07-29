package ts2mls.types

abstract class TSFieldType(members: Map[String, TSType], parents: List[TSType]) extends TSType {
  private def findInParents(name: String): Option[TSType] = parents.foldLeft[Option[TSType]](None)((res, p) => res match {
    case None => try {Some(p.>(name))} catch {case e: Exception => None}
    case _ => res
  })

  override def >(fieldName: String): TSType = members.get(fieldName) match {
    case Some(t) => t
    case _ => findInParents(fieldName) match {
      case Some(t) => t
      case _ => throw new java.lang.Exception(s"Field \"$fieldName\" not found.")
    }
  }
}
