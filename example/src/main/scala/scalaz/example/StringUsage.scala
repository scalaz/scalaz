package scalaz.example

import scalaz._

object StringUsage {
  def main(args: Array[String]): Unit = {
    import std.string._

    assert(charsNel("foo").isJust)
    assert(charsNel("").isEmpty)

    import stringSyntax._

    assert("foo".charsNel.isJust)
    assert("".charsNel.isEmpty)
  }
}
