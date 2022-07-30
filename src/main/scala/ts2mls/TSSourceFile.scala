package ts2mls

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import types._

class TSSourceFile(sf: js.Dynamic, global: TSNamespace)(implicit checker: TSTypeChecker) extends Module {
  override def >(name: String): TSType = global.>(name)
  override def >>(name: String): TSNamespace = global.>>(name)

  private def visit(node: js.Dynamic): Unit = {
    val nodeObject = TSNodeObject(node)
    if (!isExported(nodeObject) || nodeObject.isToken) return

    if (nodeObject.isFunctionDeclaration) {
      val funcName = nodeObject.symbol.escapedName
      val typeInfo = getFunctionType(nodeObject)
      global.put(funcName, typeInfo)
    }
    else if (nodeObject.isClassDeclaration) {
      val className = nodeObject.symbol.escapedName
      val typeInfo = parseClassMembers(nodeObject)(global)
      global.put(className, typeInfo)
    }
    else if (nodeObject.isInterfaceDeclaration) {
      val iName = nodeObject.symbol.escapedName
      val typeInfo = parseInterfaceMembers(nodeObject)(global)
      global.put(iName, typeInfo)
    }
    else if (nodeObject.isNamespace) {
      val nsName = nodeObject.symbol.escapedName
      parseNamespace(nodeObject)(global)
    }
  }

  TypeScript.forEachChild(sf, visit)

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
    if (res.isTupleType)
      new TSFunctionType(pList, new TSTupleType(getTupleElements(res.resolvedTypeArguments)), getTypeConstraints(node))
    else if (res.isArrayType)
      new TSFunctionType(pList, new TSArrayType(getElementType(res.resolvedTypeArguments.head())), getTypeConstraints(node))
    else if (!res.aliasSymbol.isUndefined)
      new TSFunctionType(pList, new TSNamedType(res.symbol.escapedName), getTypeConstraints(node))
    else if (!res.symbol.isUndefined && dec != null && !dec.isUndefined)
      new TSFunctionType(pList, getFunctionType(dec), getTypeConstraints(node))
    else if (res.intrinsicName != null)
      new TSFunctionType(pList, new TSNamedType(res.intrinsicName), getTypeConstraints(node))
    else if (!res.types.isUndefined)
      new TSFunctionType(pList, getUnionType(res.types), getTypeConstraints(node))
    else
      throw new java.lang.Exception("Unknown return value.")
  }

  private def getElementType(token: TSTokenObject): TSType = {
    val tp = token.`type`
    if (!tp.isUndefined && tp.isFunctionTypeNode) getFunctionType(tp)
    else if (token.isArrayTypeNode) new TSArrayType(getElementType(token.elementType))
    else if (token.isTupleTypeNode) new TSTupleType(getTupleElements(token.elements))
    else
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

  private def getTupleElements(elements: TSTokenArray): List[TSType] = {
    val tail = elements.tail()
    if (tail.isUndefined) List()
    else getTupleElements(elements) :+ getElementType(tail)
  }

  private def getFunctionParameterType(node: TSNodeObject): TSType = {
    val typeNode = node.`type`
    if (!typeNode.isUndefined && typeNode.isFunctionTypeNode) getFunctionType(typeNode)
    else if (!typeNode.isUndefined && typeNode.isArrayTypeNode) new TSArrayType(getElementType(typeNode.elementType))
    else if (!typeNode.isUndefined && !typeNode.types.isUndefined && typeNode.types.length() > 1) getUnionType(typeNode.types)
    else if (!typeNode.isUndefined && typeNode.isTupleTypeNode) new TSTupleType(getTupleElements(typeNode.elements))
    else getNamedType(node.symbol)
  }

  private def getFunctionParametersType(list: TSNodeArray): List[TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) List() else getFunctionParametersType(list) :+ getFunctionParameterType(tail)
  }

  private def getInheritList(list: TSNodeArray)(implicit ns: TSNamespace): List[TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) List()
    else getInheritList(list) :+ ns.>(tail.types.head().expression.escapedText)

  }

  private def getInheritList(node: TSNodeObject)(implicit ns: TSNamespace): List[TSType] = {
    if (node.heritageClauses.isUndefined) List()
    else getInheritList(node.heritageClauses)
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

  private def parseClassMembers(node: TSNodeObject)(implicit ns: TSNamespace): TSClassType = {
    val name = node.symbol.escapedName
    val members = node.symbol.valueDeclaration.members
    val mList = getClassMembersType(members)
    new TSClassType(name, mList, getTypeConstraints(node), getInheritList(node))
  }

  private def parseInterfaceMembers(node: TSNodeObject)(implicit ns: TSNamespace): TSInterfaceType = {
    val name = node.symbol.escapedName
    val members = node.members
    val pList = getInterfacePropertiesType(members)
    new TSInterfaceType(name, pList, getTypeConstraints(node), getInheritList(node))
  }

  private def parseNamespaceExports(it: TSSymbolIter)(implicit ns: TSNamespace): Unit = {
    it.next()
    if (!it.done) {
      val data = it.value()
      val name = data._1
      val node = data._2.getFirstDeclaration()

      if (!node.isToken && node.isFunctionDeclaration) {
        ns.put(name, getFunctionType(node))
        parseNamespaceExports(it)
      }
      else if (!node.isToken && node.isClassDeclaration) {
        ns.put(name, parseClassMembers(node))
        parseNamespaceExports(it)
      }
      else if (!node.isToken && node.isInterfaceDeclaration) {
        ns.put(name, parseInterfaceMembers(node))
        parseNamespaceExports(it)
      }
      else if (!node.isToken && node.isNamespace) {
        parseNamespace(node)
        parseNamespaceExports(it)
      }
      else parseNamespaceExports(it)
    }
  }

  private def parseNamespace(node: TSNodeObject)(implicit ns: TSNamespace): Unit = {
    val name = node.symbol.escapedName
    val iterator = node.symbol.exports
    val sub = ns.derive(name)
    parseNamespaceExports(iterator)(sub)
  }
}

object TSSourceFile {
  def apply(sf: js.Dynamic, global: TSNamespace)(implicit checker: TSTypeChecker) = new TSSourceFile(sf, global)
}