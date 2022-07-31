package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import js.JSConverters._

object TypeScript {
  private lazy val ts = g.require("typescript")

  lazy val typeFlagsUnion = ts.TypeFlags.Union.asInstanceOf[Int]

  def isToken(node: js.Dynamic): Boolean = ts.isToken(node)
  def isFunctionDeclaration(node: js.Dynamic): Boolean = ts.isFunctionDeclaration(node)
  def isClassDeclaration(node: js.Dynamic): Boolean = ts.isClassDeclaration(node)
  def isInterfaceDeclaration(node: js.Dynamic): Boolean = ts.isInterfaceDeclaration(node)
  def isFunctionTypeNode(node: js.Dynamic): Boolean = ts.isFunctionTypeNode(node)
  def isFunctionLike(node: js.Dynamic): Boolean = ts.isFunctionLike(node)
  def isArrayTypeNode(node: js.Dynamic): Boolean = ts.isArrayTypeNode(node)
  def isMethodDeclaration(node: js.Dynamic): Boolean = ts.isMethodDeclaration(node)
  def isNamespaceDeclaration(node: js.Dynamic): Boolean = ts.isModuleDeclaration(node)
  def isTupleTypeNode(node: js.Dynamic): Boolean = ts.isTupleTypeNode(node)
  def isUnionTypeNode(node: js.Dynamic): Boolean = ts.isUnionTypeNode(node)

  def forEachChild(root: js.Dynamic, func: js.Dynamic => Unit): Unit = ts.forEachChild(root, func)
  def createProgram(filenames: Seq[String]): js.Dynamic = ts.createProgram(filenames.toJSArray, js.Dictionary("maxNodeModuleJsDepth" -> 0))
}

class TSTypeChecker(checker: js.Dynamic) {
  def getTypeOfSymbolAtLocation(sym: js.Dynamic): String =
    checker.typeToString(checker.getTypeOfSymbolAtLocation(sym, sym.valueDeclaration)).toString

  def getSignatureFromDeclaration(node: js.Dynamic) = checker.getSignatureFromDeclaration(node)
  def getReturnTypeOfSignature(signature: js.Dynamic) = checker.getReturnTypeOfSignature(signature)
  def getTypeFromTypeNode(token: js.Dynamic) = TSTypeObject(checker.getTypeFromTypeNode(token))
}

object TSTypeChecker {
  def apply(checker: js.Dynamic) = new TSTypeChecker(checker)
}

class TSSymbolObject(sym: js.Dynamic) extends TSAny(sym) {
  private lazy val declarations = TSNodeArray(sym.declarations)
  lazy val escapedName: String = sym.escapedName.toString
  lazy val valueDeclaration: TSNodeObject = TSNodeObject(sym.valueDeclaration)
  lazy val exports = TSSymbolIter(sym.exports.entries())

  def getType()(implicit checker: TSTypeChecker): String = checker.getTypeOfSymbolAtLocation(sym)
  def getFirstDeclaration(): TSNodeObject = TSNodeObject(sym.declarations.shift())
}

object TSSymbolObject {
  def apply(node: js.Dynamic) = new TSSymbolObject(node)
}

case class TSNodeObject(node: js.Dynamic) extends TSAny(node) {
  lazy val isToken: Boolean = TypeScript.isToken(node)
  lazy val isFunctionDeclaration: Boolean = !isUndefined && TypeScript.isFunctionDeclaration(node)
  lazy val isClassDeclaration: Boolean = !isUndefined && TypeScript.isClassDeclaration(node)
  lazy val isInterfaceDeclaration: Boolean = !isUndefined && TypeScript.isInterfaceDeclaration(node)
  lazy val isFunctionTypeNode: Boolean = !isUndefined && TypeScript.isFunctionTypeNode(node)
  lazy val isFunctionLike: Boolean = !isUndefined && TypeScript.isFunctionLike(node)
  lazy val isArrayTypeNode: Boolean = !isUndefined && TypeScript.isArrayTypeNode(node)
  lazy val isMethodDeclaration: Boolean = !isUndefined && TypeScript.isMethodDeclaration(node)
  lazy val isNamespace: Boolean = !isUndefined && TypeScript.isNamespaceDeclaration(node)
  lazy val isTupleTypeNode: Boolean = !isUndefined && TypeScript.isTupleTypeNode(node)
  lazy val isUnionTypeNode: Boolean = !isUndefined && TypeScript.isUnionTypeNode(node)
  lazy val isEnumTypeNode: Boolean = !isUndefined && !typeName.isUndefined
  lazy val isNull: Boolean = node == null
  lazy val flags: Int = node.flags.asInstanceOf[Int]

  lazy val typeName = TSIdentifierObject(node.typeName)
  lazy val parent: TSNodeObject = if (!isNull) TSNodeObject(node.parent) else throw new java.lang.Exception("Access parent of null node.")
  lazy val symbol: TSSymbolObject = TSSymbolObject(node.symbol)
  lazy val parameters = TSNodeArray(node.parameters)
  lazy val typeParameters = TSNodeArray(node.typeParameters)
  lazy val constraint: TSTokenObject = TSTokenObject(node.constraint)
  lazy val members = TSNodeArray(node.members)
  lazy val types = TSTokenArray(node.types)
  lazy val elementType = TSTokenObject(node.elementType)
  lazy val heritageClauses = TSNodeArray(node.heritageClauses)
  lazy val elements = TSTokenArray(node.elements)
  lazy val typeToken = TSTokenObject(node.selectDynamic("type")) // for interfaces

  def getReturnTypeOfSignature()(implicit checker: TSTypeChecker): TSTypeObject = {
    val signature = checker.getSignatureFromDeclaration(node)
    TSTypeObject(checker.getReturnTypeOfSignature(signature))
  }

  // def getTypeFromTypeNode()(implicit checker: TSTypeChecker): String = checker.getTypeFromTypeNode(node)

  private def getTypeField(t: TSNodeObject): TSNodeObject =
    if (t.isUndefined || t.`type`.isUndefined || t.`type`.isToken) t else t.`type`

  def `type`(): TSNodeObject = getTypeField(TSNodeObject(node.selectDynamic("type")))
}

object TSNodeObject {
  def apply(node: js.Dynamic) = new TSNodeObject(node)
}

class TSTokenObject(token: js.Dynamic) extends TSAny(token) {
  lazy val intrinsicName: String = if (js.isUndefined(token.intrinsicName)) null else token.intrinsicName.toString
  lazy val `type`: TSNodeObject = TSNodeObject(token.selectDynamic("type"))
  lazy val expression: TSIdentifierObject = TSIdentifierObject(token.expression)
  lazy val elementType: TSNodeObject = TSNodeObject(token.elementType)
  lazy val elements = TSTokenArray(token.elements)

  lazy val isArrayTypeNode: Boolean = TypeScript.isArrayTypeNode(token)
  lazy val isTupleTypeNode: Boolean = TypeScript.isTupleTypeNode(token)

  def getTypeFromTypeNode()(implicit checker: TSTypeChecker): TSTypeObject = checker.getTypeFromTypeNode(token)
}

object TSTokenObject {
  def apply(token: js.Dynamic) = new TSTokenObject(token)
}

class TSTypeObject(obj: js.Dynamic) extends TSAny(obj) {
  lazy val symbol: TSSymbolObject = TSSymbolObject(obj.symbol)
  lazy val resolvedTypeArguments = TSTypeArray(obj.resolvedTypeArguments)
  lazy val intrinsicName: String = if (js.isUndefined(obj.intrinsicName)) null else obj.intrinsicName.toString
  lazy val aliasSymbol: TSSymbolObject = TSSymbolObject(obj.aliasSymbol)
  lazy val types = TSTypeArray(obj.types)

  lazy val flags = obj.flags.asInstanceOf[Int]

  lazy val isTupleType: Boolean = obj.checker.isTupleType(obj)
  lazy val isArrayType: Boolean = obj.checker.isArrayType(obj)
  lazy val isEnumType: Boolean = !aliasSymbol.isUndefined
  lazy val isUnionType: Boolean = flags == TypeScript.typeFlagsUnion
  lazy val declaration: TSNodeObject = if (symbol.isUndefined) TSNodeObject(obj.symbol) else symbol.getFirstDeclaration()
}

object TSTypeObject {
  def apply(obj: js.Dynamic) = new TSTypeObject(obj)
}

class TSIdentifierObject(id: js.Dynamic) extends TSAny(id) {
  lazy val escapedText: String = id.escapedText.toString
}

object TSIdentifierObject {
  def apply(id: js.Dynamic) = new TSIdentifierObject(id)
}