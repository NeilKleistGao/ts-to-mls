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

  private var types: HashMap[String, TSType] = new HashMap[String, TSType]()

  def this() = {
    this()
    generateInterfaceTypeInfo()
  }

  private def generateInterfaceTypeInfo() = {
    def visit(node: ts.Node): Unit = {
      if (!isExported(node)) return

      if (ts.isFunctionDeclaration(node))
        val funcName = node.symbol.escapedName
        val typeInfo = getFunctionType(node)
        types += funcName -> typeInfo
    }

    ts.forEachChild(sourceFile, visit _)
  }

  private def isExported(node: ts.Node) = (ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Export) != 0 ||
    (node.parent != null && node.parent.kind == ts.SyntaxKind.SourceFile)
  
  private def getPrimitiveTypeString(sym: ts.Symbol): String =
    checker.typeToString(checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration))

  private def getFunctionType(node: ts.Node): TSType = {
    val params = node.symbol.valueDeclaration.parameters
    val pList = if (params.length == 0) List() else getFunctionParametersType()
  }

  private def getFunctionParametersType(list: js.Dynamic): List[TSType] = {
    val tail = list.pop()
    val tailType = getPrimitiveTypeString(tail.symbol) // TODO: we assumed that all parameters are primitive type
    if (tail == js.undefined) List() else getFunctionParametersType(list) :+ tailType
  }
}

object TSProgram {
    def apply(filename: String) = new TSProgram(filename)
}