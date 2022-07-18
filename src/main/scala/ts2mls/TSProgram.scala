package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._

class TSProgram(filename: String) {
  private val ts: js.Dynamic = g.require("typescript")
  private val program: js.Dynamic = ts.createProgram(js.Array(filename), js.Dictionary("maxNodeModuleJsDepth" -> 0))
  private val checker: js.Dynamic = program.getTypeChecker()
  private val sourceFile: js.Dynamic = program.getSourceFile(filename)
}

object TSProgram {
    def apply(filename: String) = new TSProgram(filename)
}