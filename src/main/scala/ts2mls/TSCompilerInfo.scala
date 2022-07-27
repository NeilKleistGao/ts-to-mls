package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._

object TypeScript {
  private lazy val ts = g.require("typescript")

  lazy val ModifierFlagsExport: Int = ts.ModifierFlags.Export.asInstanceOf[Int]
  lazy val SyntaxKindSourceFile: Int = ts.SyntaxKind.SourceFile.asInstanceOf[Int]

  def isToken(node: js.Dynamic): Boolean = ts.isToken(node)
  def isFunctionDeclaration(node: js.Dynamic): Boolean = ts.isFunctionDeclaration(node)
  def isClassDeclaration(node: js.Dynamic): Boolean = ts.isClassDeclaration(node)
  def isInterfaceDeclaration(node: js.Dynamic): Boolean = ts.isInterfaceDeclaration(node)
  def getCombinedModifierFlags(node: js.Dynamic): Int = ts.getCombinedModifierFlags(node).asInstanceOf[Int]
}

class TSSymbolObject(node: js.Dynamic) {
  lazy val escapedName: String = node.escapedName.toString
}

object TSSymbolObject {
  def apply(node: js.Dynamic) = new TSSymbolObject(node)
}

class TSNodeObject(node: js.Dynamic) {
  lazy val isToken: Boolean = TypeScript.isToken(node)
  lazy val isFunctionDeclaration: Boolean = TypeScript.isFunctionDeclaration(node)
  lazy val isClassDeclaration: Boolean = TypeScript.isClassDeclaration(node)
  lazy val isInterfaceDeclaration: Boolean = TypeScript.isInterfaceDeclaration(node)
  lazy val hasExportModifier: Boolean = (TypeScript.getCombinedModifierFlags(node) & TypeScript.ModifierFlagsExport) != 0
  lazy val isNull: Boolean = node == null
  lazy val kind: Int = node.kind.asInstanceOf[Int]
  lazy val isSourceFile: Boolean = kind == TypeScript.SyntaxKindSourceFile

  lazy val parent: TSNodeObject = if (!isNull) TSNodeObject(node.parent) else throw new java.lang.Exception("Access parent of null node.")
  lazy val symbol: TSSymbolObject = TSSymbolObject(node.symbol)
}

object TSNodeObject {
  def apply(node: js.Dynamic) = new TSNodeObject(node)
}