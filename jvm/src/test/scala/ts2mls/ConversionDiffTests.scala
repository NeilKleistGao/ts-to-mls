package ts2mls

import mlscript.DiffTests

class ConversionDiffTests extends DiffTests(ConversionDiffTests.dir) {
}


object ConversionDiffTests {
  private val dir = os.pwd/"js"/"src"/"test"/"diff"
}