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
      if (!isExported(node) || ts.isToken(node)) return

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
      else if (ts.isInterfaceDeclaration(node)) {
        val iName = node.symbol.escapedName.toString
        val typeInfo = parseInterfaceMembers(node)
        types += iName -> typeInfo
      }
      else if (node.symbol.exports != js.undefined) {
        val nsName = node.symbol.escapedName.toString
        val typeInfo = parseNamespace(node)
        types += nsName -> typeInfo
      }
    }

    ts.forEachChild(sourceFile, visit _)
  }

  private def isExported(node: ts.Node) = ((ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Export) != 0 ||
    (node.parent != null && node.parent.kind == ts.SyntaxKind.SourceFile))
  
  private def getNamedType(sym: ts.Symbol): TSNamedType =
    new TSNamedType(checker.typeToString(checker.getTypeOfSymbolAtLocation(sym, sym.valueDeclaration)).toString)

  private def getFunctionType(node: ts.Node): TSFunctionType = {
    val params = node.parameters
    val pList = if (params == js.undefined) List() else getFunctionParametersType(params)
    val signature = checker.getSignatureFromDeclaration(node)
    val res = checker.getReturnTypeOfSignature(signature)
    if (res.symbol != js.undefined && res.symbol.declarations.length > 0) {
      if (res.resolvedTypeArguments != js.undefined)
        new TSFunctionType(pList, new TSArrayType(getElementType(res.resolvedTypeArguments.shift())))
      else
        new TSFunctionType(pList, getFunctionType(res.symbol.declarations.shift()))
    }
    else
      new TSFunctionType(pList, new TSNamedType(res.intrinsicName.toString))
  }

  private def getElementType(token: js.Dynamic): TSType = {
    val tp = token.selectDynamic("type")
    if (tp != js.undefined) getFunctionType(tp.symbol.declarations.shift())
    else
      if (token.intrinsicName != js.undefined) new TSNamedType(token.intrinsicName.toString) 
      else new TSNamedType(checker.getTypeFromTypeNode(token).intrinsicName.toString)
  }

  private def getFunctionParameterType(node: ts.node): TSType = {
    val typeNode = node.selectDynamic("type")
    // TODO: add more types
    if (typeNode != js.undefined && ts.isFunctionTypeNode(typeNode)) getFunctionType(typeNode)
    else if (typeNode != js.undefined && ts.isArrayTypeNode(typeNode)) new TSArrayType(getElementType(typeNode.elementType))
    else getNamedType(node.symbol)
  }

  private def getFunctionParametersType(list: js.Dynamic): List[TSType] = {
    val tail = list.pop()
    if (tail == js.undefined) List() else getFunctionParametersType(list) :+ getFunctionParameterType(tail)
  }

  private def getClassMembersType(list: js.Dynamic): Map[String, TSType] = {
    val tail = list.pop()
    if (tail == js.undefined) Map()
    else {
      val name = tail.symbol.escapedName.toString
      if (ts.isMethodDeclaration(tail) && !name.equals("__constructor")) // TODO: we assumed that there is no inner class
        getClassMembersType(list) ++ Map(name -> getFunctionType(tail))
      else getClassMembersType(list)
    }
  }

  private def getInterfacePropertiesType(list: js.Dynamic): Map[String, TSType] = {
    val tail = list.pop()
    if (tail == js.undefined) Map()
    else {
      val name = tail.symbol.escapedName.toString
      val typeObject = tail.symbol.valueDeclaration.selectDynamic("type")
      if (typeObject.parameters != js.undefined)
        getInterfacePropertiesType(list) ++ Map(name -> getFunctionType(typeObject))
      else getInterfacePropertiesType(list) ++ Map(name -> getNamedType(tail.symbol))
    }
  }

  private def parseClassMembers(node: ts.Node): TSClassType = {
    val name = node.symbol.escapedName.toString
    val members = node.symbol.valueDeclaration.members
    val mList = getClassMembersType(members)
    new TSClassType(name, mList)
  }

  private def parseInterfaceMembers(node: ts.Node): TSInterfaceType = {
    val name = node.symbol.escapedName.toString
    val members = node.members
    val pList = getInterfacePropertiesType(members)
    new TSInterfaceType(name, pList)
  }

  private def parseNamespaceExports(it: js.Dynamic): Map[String, TSType] = {
    val next = it.next()
    if (next.done) Map()
    else {
      val data = next.value
      val name = data.shift().toString
      val node = data.shift().declarations.shift()

      if (ts.isFunctionDeclaration(node)) parseNamespaceExports(it) ++ Map(name -> getFunctionType(node))
      else if (ts.isClassDeclaration(node)) parseNamespaceExports(it) ++ Map(name -> parseClassMembers(node))
      else if (ts.isInterfaceDeclaration(node)) parseNamespaceExports(it) ++ Map(name -> parseInterfaceMembers(node))
      else if (node.symbol.exports != js.undefined) parseNamespaceExports(it) ++ Map(name -> parseNamespace(node))
      else parseNamespaceExports(it)
    }
  }

  private def parseNamespace(node: ts.Node): TSNamespaceType = {
    val name = node.symbol.escapedName.toString
    val iterator = node.symbol.exports.entries()
    new TSNamespaceType(name, parseNamespaceExports(iterator))
  }

  def getType(name: String): TSType = types.getOrElse(name, throw new java.lang.Exception(s"Symbol \"$name\" not found."))
}

object TSProgram {
    def apply(filename: String) = new TSProgram(filename)
}