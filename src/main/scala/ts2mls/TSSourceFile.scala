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
      val typeInfo = getFunctionType(nodeObject)(Map())
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

  private def getObjectType(node: TSTypeSource)(implicit tv: Map[String, TSTypeVariable]): TSType = node match {
    case node: TSNodeObject => {
      val res = {
        val typeNode = node.`type`
        if (typeNode.hasTypeName) {
          val name = typeNode.typeName.escapedText
          if (tv.contains(name)) tv(name)
          else TSNamedType(name)
        }
        else if (typeNode.isFunctionTypeNode) getFunctionType(typeNode)
        else if (typeNode.isTupleTypeNode) TSTupleType(getTupleElements(typeNode.elements))
        else if (typeNode.isUnionTypeNode) getUnionType(typeNode.typesToken, None)
        else if (typeNode.isIntersectionTypeNode) getIntersectionType(typeNode.types, None)
        else if (typeNode.isArrayTypeNode) TSArrayType(getObjectType(typeNode.elementType.getTypeFromTypeNode))
        else if (!node.typeName.isUndefined) TSTypeVariable(node.typeName.escapedText, None)
        else if (!typeNode.isUndefined && !typeNode.members.isUndefined)
          TSInterfaceType("", getInterfacePropertiesType(typeNode.members), List(), List())
        else {
          val name = node.symbol.getType()
          if (tv.contains(name)) tv(name)
          else TSNamedType(name)
        }
      }
      
      if (node.questionToken.isUndefined) res
      else TSUnionType(res, TSNamedType("undefined"))
    }
    case obj: TSTypeObject => {
      val dec = obj.declaration
      if (obj.isEnumType) TSNamedType(obj.aliasSymbol.escapedName)
      else if (dec.isFunctionLike) getFunctionType(dec)
      else if (obj.isTupleType) TSTupleType(getTupleElements(obj.resolvedTypeArguments))
      else if (obj.isUnionType) getStructuralType(obj.types, None, true)
      else if (obj.isIntersectionType) getStructuralType(obj.types, None, false)
      else if (obj.isArrayType) TSArrayType(getObjectType(obj.resolvedTypeArguments.head()))
      else if (!obj.symbol.isUndefined) {
        val symDec = obj.symbol.valueDeclaration
        if (symDec.isUndefined || symDec.properties.isUndefined) TSTypeVariable(obj.symbol.escapedName, None)
        else TSInterfaceType("", getInterfacePropertiesType(symDec.properties), List(), List())
      }
      else 
        if (tv.contains(obj.intrinsicName)) tv(obj.intrinsicName)
        else TSNamedType(obj.intrinsicName)
    }
  }

  private def getTypeConstraints(list: TSNodeArray, prev: List[TSTypeVariable])(implicit tv: Map[String, TSTypeVariable]): List[TSTypeVariable] = {
    val tail = list.tail()
    if (tail.isUndefined) prev
    else if (tail.constraint.isUndefined)
      getTypeConstraints(list, prev) :+ TSTypeVariable(tail.symbol.escapedName, None)
    else
      getTypeConstraints(list, prev) :+ TSTypeVariable(tail.symbol.escapedName, Some(getObjectType(tail.constraint.getTypeFromTypeNode)))
  }

  private def getTypeConstraints(node: TSNodeObject)(implicit tv: Map[String, TSTypeVariable]): List[TSTypeVariable] = {
    if (node.typeParameters.isUndefined) List()
    else getTypeConstraints(node.typeParameters, List())
  }

  private def getFunctionParametersType(list: TSNodeArray)(implicit tv: Map[String, TSTypeVariable]): List[TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) List() else getFunctionParametersType(list) :+ getObjectType(tail)
  }

  private def constaintsListToMap(constraints: List[TSTypeVariable]) =
    constraints.foldLeft(Map[String, TSTypeVariable]())((m, v) => 
      m ++ Map(v.name -> TSTypeVariable(v.name, None)) // we will apply the constraints in the record declarations.
    )

  private def getFunctionType(node: TSNodeObject)(implicit tv: Map[String, TSTypeVariable]): TSFunctionType = {
    val params = node.parameters
    val constraints = getTypeConstraints(node)
    val ntv = constaintsListToMap(constraints) ++ tv
    val pList = if (params.isUndefined) List() else getFunctionParametersType(params)(ntv)
    val res = node.getReturnTypeOfSignature()

    TSFunctionType(pList, getObjectType(res)(ntv), constraints)
  }

  private def getUnionType(types: TSTokenArray, prev: Option[TSUnionType])(implicit tv: Map[String, TSTypeVariable]): TSUnionType = prev match {
    case None => {
      val fst = types.head()
      val snd = types.head()
      getUnionType(types, Some(TSUnionType(getObjectType(fst.getTypeFromTypeNode), getObjectType(snd.getTypeFromTypeNode))))
    }
    case _ => {
      val t = types.head()
      if (t.isUndefined) prev.get
      else getUnionType(types, Some(TSUnionType(prev.get, getObjectType(t.getTypeFromTypeNode))))
    }
  }

  private def getIntersectionType(types: TSNodeArray, prev: Option[TSIntersectionType])(implicit tv: Map[String, TSTypeVariable]): TSIntersectionType = prev match {
    case None => {
      val fst = types.head()
      val snd = types.head()
      getIntersectionType(types, Some(TSIntersectionType(getObjectType(fst), getObjectType(snd))))
    }
    case _ => {
      val t = types.head()
      if (t.isUndefined) prev.get
      else getIntersectionType(types, Some(TSIntersectionType(prev.get, getObjectType(t))))
    }
  }

  private def getStructuralType(types: TSTypeArray, prev: Option[TSStructuralType], isUnion: Boolean)(implicit tv: Map[String, TSTypeVariable]): TSStructuralType = prev match {
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

  private def getTupleElements(elements: TSTokenArray)(implicit tv: Map[String, TSTypeVariable]): List[TSType] = {
    val tail = elements.tail()
    if (tail.isUndefined) List()
    else getTupleElements(elements) :+ getObjectType(tail.getTypeFromTypeNode)
  }

  private def getTupleElements(elements: TSTypeArray)(implicit tv: Map[String, TSTypeVariable]): List[TSType] = {
    val tail = elements.tail()
    if (tail.isUndefined) List()
    else getTupleElements(elements) :+ getObjectType(tail)
  }

  private def getInheritList(list: TSNodeArray)(implicit ns: TSNamespace): List[TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) List()
    else getInheritList(list) :+ ns.>(tail.typesToken.head().expression.escapedText)
  }

  private def getInheritList(node: TSNodeObject)(implicit ns: TSNamespace): List[TSType] = {
    if (node.heritageClauses.isUndefined) List()
    else getInheritList(node.heritageClauses)
  }

  private def getClassMembersType(list: TSNodeArray)(implicit tv: Map[String, TSTypeVariable]): Map[String, TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) Map()
    else {
      val name = tail.symbol.escapedName
      if (tail.isMethodDeclaration && !name.equals("__constructor"))
        getClassMembersType(list) ++ Map(name -> getFunctionType(tail))
      else getClassMembersType(list)
    }
  }

  private def getInterfacePropertiesType(list: TSNodeArray)(implicit tv: Map[String, TSTypeVariable]): Map[String, TSType] = {
    val tail = list.tail()
    if (tail.isUndefined) Map()
    else {
      val name = tail.symbol.escapedName
      getInterfacePropertiesType(list) ++ Map(name -> getObjectType(tail))
    }
  }

  private def parseMembers(node: TSNodeObject, isClass: Boolean)(implicit ns: TSNamespace): TSFieldType = {
    val name = node.symbol.escapedName
    val members = node.members
    val constraints = getTypeConstraints(node)(Map())
    val tvMap = constaintsListToMap(constraints)

    if (isClass) TSClassType(name, getClassMembersType(members)(tvMap), constraints, getInheritList(node))
    else TSInterfaceType(name, getInterfacePropertiesType(members)(tvMap), constraints, getInheritList(node))
  }

  private def parseNamespaceExports(it: TSSymbolIter)(implicit ns: TSNamespace): Unit = {
    it.next()
    if (!it.done) {
      val data = it.value()
      val name = data._1
      val node = data._2.getFirstDeclaration()

      if (!node.isToken && node.isFunctionDeclaration) {
        ns.put(name, getFunctionType(node)(Map()))
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