package scalaz.example

import scalaz._

object StringUsage extends App{
  import std.string._

  assert(charsNel("foo").isJust)
  assert(charsNel("").isEmpty)

  import stringSyntax._

  assert("foo".charsNel.isJust)
  assert("".charsNel.isEmpty)
}
