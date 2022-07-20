package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import scala.collection.mutable.HashMap
import ts2mls.types._

class TSProgram(filename: String) {
  private val ts: js.Dynamic = g.require("typescript")
  private val program: js.Dynamic = ts.createProgram(js.Array(filename), js.Dictionary("maxNodeModuleJsDepth" -> 0))
  private val checker: js.Dynamic = program.getTypeChecker()
  private val sourceFile: js.Dynamic = program.getSourceFile(filename)

  private var types: HashMap[String, TSType]

  def this() = {
    this()
    generateInterfaceTypeInfo()
  }

  private def generateInterfaceTypeInfo() = {
    def visit(node: ts.Node): Unit = {

    }

    ts.forEachChild(sourceFile, visit _)
  }
}

object TSProgram {
    def apply(filename: String) = new TSProgram(filename)
}