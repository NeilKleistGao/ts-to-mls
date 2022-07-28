package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import scala.collection.mutable.HashMap
import ts2mls.types._

class TSProgram(filename: String) {
  private val program = TypeScript.createProgram(filename)
  implicit private val checker: TSTypeChecker = TSTypeChecker(program.getTypeChecker())
  private val sourceFile = program.getSourceFile(filename)

  private var types: HashMap[String, TSType] = new HashMap[String, TSType]()

  generateInterfaceTypeInfo()

  private def generateInterfaceTypeInfo() = {
    def visit(node: js.Dynamic): Unit = {
      val nodeObject = TSNodeObject(node)
      if (!isExported(nodeObject) || nodeObject.isToken) return

      if (nodeObject.isFunctionDeclaration) {
        val funcName = nodeObject.symbol.escapedName
        val typeInfo = getFunctionType(nodeObject)
        types += funcName -> typeInfo
      }
      else if (nodeObject.isClassDeclaration) {
        val className = nodeObject.symbol.escapedName
        val typeInfo = parseClassMembers(nodeObject)
        types += className -> typeInfo
      }
      else if (nodeObject.isInterfaceDeclaration) {
        val iName = nodeObject.symbol.escapedName
        val typeInfo = parseInterfaceMembers(nodeObject)
        types += iName -> typeInfo
      }
      else if (!node.symbol.exports.isUndefined) {
        val nsName = nodeObject.symbol.escapedName
        val typeInfo = parseNamespace(nodeObject)
        types += nsName -> typeInfo
      }
    }

    TypeScript.forEachChild(sourceFile, visit)
  }

  private def isExported(node: TSNodeObject) = (node.hasExportModifier || (!node.parent.isNull && node.parent.isSourceFile))
  
  private def getNamedType(sym: TSSymbolObject): TSNamedType = new TSNamedType(sym.getType())

  private def getTypeConstraints(list: TSNodeArray, prev: Map[String, TSType]): Map[String, TSType] = {
    val tail = list.head()
    if (tail.isUndefined) prev
    else if (tail.constraint.isUndefined) getTypeConstraints(list, prev)
    else getTypeConstraints(list, prev) ++ Map(tail.symbol.escapedName.toString() -> getElementType(tail.constraint))
  }

  private def getTypeConstraints(node: TSNodeObject): Map[String, TSType] = {
    if (node.typeParameters.isUndefined) Map()
    else getTypeConstraints(node.typeParameters, Map())
  }

  private def getFunctionType(node: TSNodeObject): TSFunctionType = {
    val params = node.parameters
    val pList = if (params.isUndefined) List() else getFunctionParametersType(params)
    val res = node.getReturnTypeOfSignature()
    val dec = if (res.symbol.isUndefined) null else res.symbol.getFirstDeclaration()
    if (!res.symbol.isUndefined && dec != null && !dec.isUndefined) {
      if (!res.resolvedTypeArguments.isUndefined)
        new TSFunctionType(pList, new TSArrayType(getElementType(res.resolvedTypeArguments.head())), getTypeConstraints(node))
      else {
        new TSFunctionType(pList, getFunctionType(dec), getTypeConstraints(node))}
    }
    else
      new TSFunctionType(pList, new TSNamedType(res.intrinsicName), getTypeConstraints(node))
  }

  private def getElementType(token: TSTokenObject): TSType = {
    if (token.intrinsicName != null) new TSNamedType(token.intrinsicName) 
    else new TSNamedType(token.getTypeFromTypeNode())
  }

  private def getElementType(node: TSNodeObject): TSType = {    
    val tp = node.`type`
    if (tp.isUndefined) new TSNamedType(node.getTypeFromTypeNode())
    else {
      val dec = tp.symbol.getFirstDeclaration()
      getFunctionType(dec)
    }
  }

  private def getUnionType(types: TSTokenArray, prev: TSUnionType): TSUnionType = {
    val t = types.tail()
    if (t.isUndefined) prev
    else getUnionType(types, new TSUnionType(getElementType(t), prev))
  }

  private def getUnionType(types: TSTokenArray): TSUnionType = {
    val snd = types.tail()
    val fst = types.tail()
    val u = new TSUnionType(getElementType(fst), getElementType(snd))
    getUnionType(types, u)
  }

  private def getFunctionParameterType(node: TSNodeObject): TSType = {
    val typeNode = node.`type`
    if (!typeNode.isUndefined && typeNode.isFunctionTypeNode) getFunctionType(typeNode)
    else if (!typeNode.isUndefined && typeNode.isArrayTypeNode) new TSArrayType(getElementType(typeNode.elementType))
    else if (!typeNode.isUndefined && !typeNode.types.isUndefined && typeNode.types.length() > 1) getUnionType(typeNode.types)
    else getNamedType(node.symbol)
  }

  private def getFunctionParametersType(list: TSNodeArray): List[TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) List() else getFunctionParametersType(list) :+ getFunctionParameterType(tail)
  }

  private def getClassMembersType(list: TSNodeArray): Map[String, TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) Map()
    else {
      val name = tail.symbol.escapedName
      if (tail.isMethodDeclaration && !name.equals("__constructor")) // TODO: we assumed that there is no inner class
        getClassMembersType(list) ++ Map(name -> getFunctionType(tail))
      else getClassMembersType(list)
    }
  }

  private def getInterfacePropertiesType(list: TSNodeArray): Map[String, TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) Map()
    else {
      val name = tail.symbol.escapedName
      val typeObject = tail.symbol.valueDeclaration.`type`
      if (!typeObject.parameters.isUndefined)
        getInterfacePropertiesType(list) ++ Map(name -> getFunctionType(typeObject))
      else getInterfacePropertiesType(list) ++ Map(name -> getNamedType(tail.symbol))
    }
  }

  private def parseClassMembers(node: TSNodeObject): TSClassType = {
    val name = node.symbol.escapedName
    val members = node.symbol.valueDeclaration.members
    val mList = getClassMembersType(members)
    new TSClassType(name, mList)
  }

  private def parseInterfaceMembers(node: TSNodeObject): TSInterfaceType = {
    val name = node.symbol.escapedName
    val members = node.members
    val pList = getInterfacePropertiesType(members)
    new TSInterfaceType(name, pList, getTypeConstraints(node))
  }

  private def parseNamespaceExports(it: TSSymbolIter): Map[String, TSType] = {
    it.next()
    if (it.done) Map()
    else {
      val data = it.value()
      val name = data._1
      val node = data._2.getFirstDeclaration()

      if (node.isFunctionDeclaration) parseNamespaceExports(it) ++ Map(name -> getFunctionType(node))
      else if (node.isClassDeclaration) parseNamespaceExports(it) ++ Map(name -> parseClassMembers(node))
      else if (node.isInterfaceDeclaration) parseNamespaceExports(it) ++ Map(name -> parseInterfaceMembers(node))
      else if (!node.symbol.exports.isUndefined) parseNamespaceExports(it) ++ Map(name -> parseNamespace(node))
      else parseNamespaceExports(it)
    }
  }

  private def parseNamespace(node: TSNodeObject): TSNamespaceType = {
    val name = node.symbol.escapedName
    val iterator = node.symbol.exports
    new TSNamespaceType(name, parseNamespaceExports(iterator))
  }

  def getType(name: String): TSType = types.getOrElse(name, throw new java.lang.Exception(s"Symbol \"$name\" not found."))
}

object TSProgram {
    def apply(filename: String) = new TSProgram(filename)
}