package scalaz

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._

class CaseInsensitiveTest extends testlib.Spec {

  "map identity" ! prop {
    (a: CaseInsensitive[String]) =>
      Equal[CaseInsensitive[String]].equal(a.map(x => x), a)
  }
  
  "map associativity" ! prop {
    (a: CaseInsensitive[String], f: String => String, g: String => String) =>
      Equal[CaseInsensitive[String]].equal(a.map(f).map(g), a.map(g compose f))
  }
  
  checkAll(monoid.laws[CaseInsensitive[String]])
  checkAll(equal.laws[CaseInsensitive[String]])
  checkAll(order.laws[CaseInsensitive[String]])
}