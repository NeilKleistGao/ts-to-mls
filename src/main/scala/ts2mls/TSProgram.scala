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

  generateInterfaceTypeInfo()

  private def generateInterfaceTypeInfo() = {
    def visit(node: ts.Node): Unit = {
      if (!isExported(node)) return

      if (ts.isFunctionDeclaration(node)) {
        val funcName = node.symbol.escapedName.toString
        val typeInfo = getFunctionType(node)
        types += funcName -> typeInfo
      }
      else if (ts.isClassDeclaration(node)) {
        val className = node.symbol.escapedName.toString
        val typeInfo = parseClassMembers(node)
        types += className -> typeInfo
      }
    }

    ts.forEachChild(sourceFile, visit _)
  }

  private def isExported(node: ts.Node) = ((ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Export) != 0 ||
    (node.parent != null && node.parent.kind == ts.SyntaxKind.SourceFile))
  
  private def getPrimitiveType(sym: ts.Symbol): TSPrimitiveType =
    new TSPrimitiveType(checker.typeToString(checker.getTypeOfSymbolAtLocation(sym, sym.valueDeclaration)).toString)

  private def getFunctionType(node: ts.Node): TSFunctionType = {
    val params = node.symbol.valueDeclaration.parameters
    val pList = if (params == js.undefined) List() else getFunctionParametersType(params)
    val signature = checker.getSignatureFromDeclaration(node.symbol.declarations.shift())
    val res = new TSPrimitiveType(checker.getReturnTypeOfSignature(signature).intrinsicName.toString)
    new TSFunctionType(pList, res)
  }

  private def getFunctionParametersType(list: js.Dynamic): List[TSType] = {
    val tail = list.pop()
    // TODO: we assumed that all parameters are primitive type
    if (tail == js.undefined) List() else getFunctionParametersType(list) :+ getPrimitiveType(tail.symbol)
  }

  private def getMembersType(list: js.Dynamic): Map[String, TSType] = {
    val tail = list.pop()
    if (tail == js.undefined) Map()
    else {
      val name = tail.symbol.escapedName.toString
      if (ts.isFunctionLike(tail) && !name.equals("__constructor")) // TODO: we assumed that there is no inner class
        getMembersType(list) ++ Map(name -> getFunctionType(tail))
      else getMembersType(list)
    }
  }

  private def parseClassMembers(node: ts.Node): TSClassType = {
    val name = node.symbol.escapedName.toString
    val members = node.symbol.valueDeclaration.members
    val mList = getMembersType(members)
    new TSClassType(name, mList)
  }

  def getType(name: String): TSType = types.getOrElse(name, throw new java.lang.Exception(s"Symbol \"$name\" not found."))
}

object TSProgram {
    def apply(filename: String) = new TSProgram(filename)
}