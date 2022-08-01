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
    if (nodeObject.isToken) return

    if (nodeObject.isFunctionDeclaration) {
      val funcName = nodeObject.symbol.escapedName
      val typeInfo = getFunctionType(nodeObject)
      global.put(funcName, typeInfo)
    }
    else if (nodeObject.isClassDeclaration) {
      val className = nodeObject.symbol.escapedName
      val typeInfo = parseMembers(nodeObject, true)(global)
      global.put(className, typeInfo)
    }
    else if (nodeObject.isInterfaceDeclaration) {
      val iName = nodeObject.symbol.escapedName
      val typeInfo = parseMembers(nodeObject, false)(global)
      global.put(iName, typeInfo)
    }
    else if (nodeObject.isNamespace) {
      val nsName = nodeObject.symbol.escapedName
      parseNamespace(nodeObject)(global)
    }
  }

  TypeScript.forEachChild(sf, visit)

  private def getNamedType(sym: TSSymbolObject): TSNamedType = TSNamedType(sym.getType())

  private def getObjectType(node: TSTypeSource): TSType = node match {
    case node: TSNodeObject => {
      val typeNode = node.`type`
      if (typeNode.isEnumTypeNode) TSNamedType(typeNode.typeName.escapedText)
      else if (typeNode.isFunctionTypeNode) getFunctionType(typeNode)
      else if (typeNode.isTupleTypeNode) TSTupleType(getTupleElements(typeNode.elements))
      else if (typeNode.isUnionTypeNode) getStructuralType(typeNode.types, None, true)
      else if (typeNode.isIntersectionTypeNode) getStructuralType(typeNode.types, None, false)
      else if (typeNode.isArrayTypeNode) TSArrayType(getObjectType(typeNode.elementType.getTypeFromTypeNode))
      else getNamedType(node.symbol)
    }
    case obj: TSTypeObject => {
      val dec = obj.declaration
      if (obj.isEnumType) TSNamedType(obj.aliasSymbol.escapedName)
      else if (dec.isFunctionLike) getFunctionType(dec)
      else if (obj.isTupleType) TSTupleType(getTupleElements(obj.resolvedTypeArguments))
      else if (obj.isUnionType) getStructuralType(obj.types, None, true)
      else if (obj.isIntersectionType) getStructuralType(obj.types, None, false)
      else if (obj.isArrayType) TSArrayType(getObjectType(obj.resolvedTypeArguments.head()))
      else TSNamedType(obj.intrinsicName)
    }
  }

  private def getTypeConstraints(list: TSNodeArray, prev: List[TSTypeVariable]): List[TSTypeVariable] = {
    val tail = list.tail()
    if (tail.isUndefined) prev
    else if (tail.constraint.isUndefined)
      getTypeConstraints(list, prev) :+ TSTypeVariable(tail.symbol.escapedName, None)
    else
      getTypeConstraints(list, prev) :+ TSTypeVariable(tail.symbol.escapedName, Some(getObjectType(tail.constraint.getTypeFromTypeNode)))
  }

  private def getTypeConstraints(node: TSNodeObject): List[TSTypeVariable] = {
    if (node.typeParameters.isUndefined) List()
    else getTypeConstraints(node.typeParameters, List())
  }

  private def getFunctionParametersType(list: TSNodeArray): List[TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) List() else getFunctionParametersType(list) :+ getObjectType(tail)
  }

  private def getFunctionType(node: TSNodeObject): TSFunctionType = {
    val params = node.parameters
    val pList = if (params.isUndefined) List() else getFunctionParametersType(params)
    val res = node.getReturnTypeOfSignature()
    TSFunctionType(pList, getObjectType(res), getTypeConstraints(node))
  }

  private def getStructuralType(types: TSTokenArray, prev: Option[TSStructuralType], isUnion: Boolean): TSStructuralType = prev match {
    case None => {
      val fst = types.head()
      val snd = types.head()
      if (isUnion)
        getStructuralType(types, Some(TSUnionType(getObjectType(fst.getTypeFromTypeNode), getObjectType(snd.getTypeFromTypeNode))), isUnion)
      else
        getStructuralType(types, Some(TSIntersectionType(getObjectType(fst.getTypeFromTypeNode), getObjectType(snd.getTypeFromTypeNode))), isUnion)
    }
    case _ => {
      val t = types.head()
      if (t.isUndefined) prev.get
      else 
        if (isUnion)
          getStructuralType(types, Some(TSUnionType(prev.get, getObjectType(t.getTypeFromTypeNode))), isUnion)
        else
          getStructuralType(types, Some(TSIntersectionType(prev.get, getObjectType(t.getTypeFromTypeNode))), isUnion)
    }
  }

  private def getStructuralType(types: TSTypeArray, prev: Option[TSStructuralType], isUnion: Boolean): TSStructuralType = prev match {
    case None => {
      val fst = types.head()
      val snd = types.head()
      if (isUnion)
        getStructuralType(types, Some(TSUnionType(getObjectType(fst), getObjectType(snd))), isUnion)
      else
        getStructuralType(types, Some(TSIntersectionType(getObjectType(fst), getObjectType(snd))), isUnion)
    }
    case _ => {
      val t = types.head()
      if (t.isUndefined) prev.get
      else 
        if (isUnion)
          getStructuralType(types, Some(TSUnionType(prev.get, getObjectType(t))), isUnion)
        else
          getStructuralType(types, Some(TSIntersectionType(prev.get, getObjectType(t))), isUnion)
    }
  }

  private def getTupleElements(elements: TSTokenArray): List[TSType] = {
    val tail = elements.tail()
    if (tail.isUndefined) List()
    else getTupleElements(elements) :+ getObjectType(tail.getTypeFromTypeNode)
  }

  private def getTupleElements(elements: TSTypeArray): List[TSType] = {
    val tail = elements.tail()
    if (tail.isUndefined) List()
    else getTupleElements(elements) :+ getObjectType(tail)
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
      if (tail.isMethodDeclaration && !name.equals("__constructor"))
        getClassMembersType(list) ++ Map(name -> getFunctionType(tail))
      else getClassMembersType(list)
    }
  }

  private def getInterfacePropertiesType(list: TSNodeArray): Map[String, TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) Map()
    else {
      val name = tail.symbol.escapedName
      val typeObject = tail.typeToken.getTypeFromTypeNode
      getInterfacePropertiesType(list) ++ Map(name -> getObjectType(typeObject))
    }
  }

  private def parseMembers(node: TSNodeObject, isClass: Boolean)(implicit ns: TSNamespace): TSFieldType = {
    val name = node.symbol.escapedName
    val members = node.members
    if (isClass) TSClassType(name, getClassMembersType(members), getTypeConstraints(node), getInheritList(node))
    else TSInterfaceType(name, getInterfacePropertiesType(members), getTypeConstraints(node), getInheritList(node))
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
        ns.put(name, parseMembers(node, true))
        parseNamespaceExports(it)
      }
      else if (!node.isToken && node.isInterfaceDeclaration) {
        ns.put(name, parseMembers(node, false))
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