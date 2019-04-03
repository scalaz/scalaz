package scalaz.example

import scalaz._

object StringUsage extends App{
  import std.string._

  assert(charsNel("foo").isDefined)
  assert(charsNel("").isEmpty)

  import stringSyntax._

  assert("foo".charsNel.isDefined)
  assert("".charsNel.isEmpty)
}
