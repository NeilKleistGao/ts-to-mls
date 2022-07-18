package ts2mls;

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._

class TSBackend {
  private implicit val ts: js.Dynamic = g.require("typescript")
}

object TSBackend {
    def apply() = new TSBackend()
}