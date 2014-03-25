package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.anyVal._, std.string._

object MonoidCoproductTest extends SpecLite {

  checkAll(monoid.laws[Int :+: String])
  checkAll(equal.laws[Int :+: String])

}
