package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object CaseInsensitiveTest extends SpecLite {

  "map identity" ! forAll {
    (a: CaseInsensitive[String]) =>
      Equal[CaseInsensitive[String]].equal(a.map(x => x), a)
  }

  "map associativity" ! forAll {
    (a: CaseInsensitive[String], f: String => String, g: String => String) =>
      Equal[CaseInsensitive[String]].equal(a.map(f).map(g), a.map(g compose f))
  }

  checkAll(monoid.laws[CaseInsensitive[String]])
  checkAll(equal.laws[CaseInsensitive[String]])
  checkAll(order.laws[CaseInsensitive[String]])
}
