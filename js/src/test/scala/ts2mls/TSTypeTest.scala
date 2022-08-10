package ts2mls

import ts2mls.types._

object TSTypeTest {
  // modify the path when needed
  private val path = "js/src/test/typescript"
  private val dPath = "js/src/test/diff"

  def tsPath(filename: String) = s"$path/$filename"

  def tsPathes(filenames: Seq[String]) = filenames.map((fn) => tsPath(fn))

  def diffPath(filename: String) = s"$dPath/$filename"
}
