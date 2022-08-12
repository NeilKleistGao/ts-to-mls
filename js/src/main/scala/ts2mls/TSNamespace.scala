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

  def containsMember(path: List[String]): Boolean = path match {
    case name :: Nil => containsMember(name)
    case sub :: rest if (subSpace.contains(sub)) => subSpace(sub).containsMember(rest)
    case _ => false
  }

  override def visit(writer: DecWriter, prefix: String): Unit = {
    subSpace.foreach((p) => p._2.visit(writer, prefix + showPrefix))
    members.foreach((p) => p._2 match {
      case inter: TSIntersectionType => writer.generate(s"def ${p._1}: ${TSProgram.getMLSType(inter)}")
      case f: TSFunctionType => {
        if (f.dbg) writer.debug(s"${prefix}$showPrefix${p._1}", f.toString)

        val params = f.typeVars.foldLeft("")((p, t) => s"$p${t.name}, ") // TODO: add constraints
        if (params.length() == 0)
          writer.generate(s"def ${p._1}: ${TSProgram.getMLSType(f)}")
        else
          writer.generate(s"def ${p._1}[${params.substring(0, params.length() - 2)}]: ${TSProgram.getMLSType(f)}")
      }
      case _ => writer.generate(TSProgram.getMLSType(p._2))
    })
  }
}

object TSNamespace {
  def apply() = new TSNamespace("globalThis", None)
}
