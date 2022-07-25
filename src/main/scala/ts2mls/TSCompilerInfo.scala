package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._

class TSSymbolObject(node: js.Dynamic, ts: js.Dynamic) {
    lazy val escapedName: String = node.escapedName.toString
    
}

object TSSymbolObject {
    def apply(node: js.Dynamic)(implicit ts: js.Dynamic) = new TSSymbolObject(node, ts)
}

class TSNodeObject(node: js.Dynamic, ts: js.Dynamic) {
    lazy val isToken: Boolean = ts.isToken(node)
    lazy val isFunctionDeclaration: Boolean = ts.isFunctionDeclaration(node)
    lazy val isClassDeclaration: Boolean = ts.isClassDeclaration(node)
    lazy val isInterfaceDeclaration: Boolean = ts.isInterfaceDeclaration(node)

    lazy val symbol: TSSymbolObject = TSSymbolObject(node.symbol)(ts)
}

object TSNodeObject {
    def apply(node: js.Dynamic)(implicit ts: js.Dynamic) = new TSNodeObject(node, ts)
}