package ts2mls

import scala.scalajs.js
import js.Dynamic.{global => g}
import js.DynamicImplicits._

class DecWriter(filename: String) {
  private val outputMarker = "//│ "

  private val out = DecWriter.fs.openSync(filename, "w+")

  def output(name: String, value: String) = {
    s"$name:\n$value".split('\n').foreach(l => DecWriter.fs.writeSync(out, s"$outputMarker$l\n"))
    DecWriter.fs.writeSync(out, "\n")
  }

  def close(): Unit = DecWriter.fs.closeSync(out)
}

object DecWriter {
  private val fs = g.require("fs")

  def apply(filename: String) = new DecWriter(filename)
}