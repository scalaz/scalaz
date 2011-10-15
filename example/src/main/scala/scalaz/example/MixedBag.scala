package scalaz.example

import scalaz._

object MixedBag extends App {
  def monoid() {
    import std.AnyVal._
    import std.Option._

    import syntax.Syntax.monoid._
    
    1 |+| 2
    1 mappend 2
    some(1) |+| some(2)
    some(1) |+| mzero[Option[Int]]

    int.semigroupSyntax.mappend(1, 2)
  }
}