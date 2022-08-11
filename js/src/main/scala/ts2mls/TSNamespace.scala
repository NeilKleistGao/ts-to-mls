package ts2mls

import scala.collection.mutable.HashMap
import types._

class TSNamespace(name: String, parent: Option[TSNamespace]) extends Module {
  private val subSpace = HashMap[String, TSNamespace]()
  private val members = HashMap[String, TSType]()

  private lazy val showPrefix = if (name.equals("globalThis")) "" else s"$name."

  def derive(name: String): TSNamespace = {
    val sub = new TSNamespace(name, Some(this))
    subSpace.put(name, sub)
    sub
  }

  def put(name: String, tp: TSType): Unit = members.put(name, tp)

  override def >(name: String): TSType = members.get(name) match {
    case Some(tst) => tst
    case None if (!parent.isEmpty) => parent.get.>(name)
    case _ => throw new java.lang.Exception(s"member $name not found.")
  }
  override def >>(name: String): TSNamespace = subSpace.getOrElse(name, throw new java.lang.Exception(s"namespace $name not found."))

  override def toString(): String = s"namespace $name"

  def containsMember(name: String): Boolean = 
    if (parent.isEmpty) members.contains(name) else (members.contains(name) || parent.get.containsMember(name))

  override def visit(writer: DecWriter, prefix: String): Unit = {
    subSpace.foreach((p) => p._2.visit(writer, prefix + showPrefix))
    members.foreach((p) => p._2 match {
      case t: TSFieldType => {
        if (p._2.dbg) writer.debug(s"${prefix}$showPrefix${p._1}", p._2.toString)
        t.visit(writer, prefix + showPrefix)
      }
      case _ => {
        if (p._2.dbg) writer.debug(s"${prefix}$showPrefix${p._1}", p._2.toString)
        writer.generate(s"def ${p._1}: ${TSProgram.getMLSType(p._2)}")
      }
    })
  }
}

object TSNamespace {
  def apply() = new TSNamespace("globalThis", None)
}
