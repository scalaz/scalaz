package scalaz

import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary}


class GroupTest extends Spec {
  addArguments(fullStackTrace)

  import Scalaz._
  import ScalazArbitrary._
  import ScalazProperties._

  implicit val booleanGroup = std.anyVal.booleanInstance.conjunction

  "function minus" in {
    ({
      x: Int => x + 1
    } |-| {
      x: Int => x + 2
    }).apply(1) must_== -1
  }

  type A = Int
  type B = Int
  type C = Int
  type D = Int
  type E = Int
  type F = Int
  type G = Int
  type H = Int

  checkAll("Int", group.laws[Int])
  checkAll("BigInt", group.laws[BigInt])
  checkAll("BigInteger", group.laws[BigInteger])
  checkAll("Short", group.laws[Short])
  checkAll("Long", group.laws[Long])
  checkAll("(A)", group.laws[(A)])
  checkAll("(A, B)", group.laws[(A, B)])
  checkAll("(A, B, C)", group.laws[(A, B, C)])
  checkAll("(A, B, C, D)", group.laws[(A, B, C, D)])
  checkAll("(A, B, C, D, E)", group.laws[(A, B, C, D, E)])
  checkAll("(A, B, C, D, E, F)", group.laws[(A, B, C, D, E, F)])
  checkAll("(A, B, C, D, E, F, G)", group.laws[(A, B, C, D, E, F, G)])
  checkAll("(A, B, C, D, E, F, G, H)", group.laws[(A, B, C, D, E, F, G, H)])
}
