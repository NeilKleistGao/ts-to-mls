import ts2mls.types._

object TSTypeTest {
  // modify the path when needed
  private val path = "js/src/test/typescript"

  def apply(t: TSType, s: String): Boolean = t.toString.equals(s)

  def tsPath(filename: String) = s"$path/$filename"

  def tsPathes(filenames: Seq[String]) = filenames.map((fn) => tsPath(fn))
}
