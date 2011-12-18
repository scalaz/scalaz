package scalaz

import java.math.BigInteger
import scalacheck.{ScalazArbitrary}
import ScalazArbitrary._
import Scalaz._
import Tags._
import scalacheck.ScalazProperties.monoid

class MonoidTest extends Spec {
  implicit val booleanMonoid = std.anyVal.booleanInstance.conjunction

  "function monoid" in {
    val f: (Int) => Int = implicitly[Semigroup[Int => Int]].append(_ + 1, _ * 2)
    f(1) must_== 4
  }

  type A = Int
  type B = Int
  type C = Int
  type D = Int
  type E = Int
  type F = Int
  type G = Int
  type H = Int

  checkAll("Boolean @@ Conjunction", monoid.laws[Boolean @@ Conjunction])
  checkAll("Boolean", monoid.laws[Boolean])
  // todo
  // checkAll("Digit", monoid.laws[Digit])
  checkAll("Int", monoid.laws[Int])
  checkAll("String", monoid.laws[String])
  checkAll("List", monoid.laws[List[Int]])
  checkAll("BigInt", monoid.laws[BigInt])
  checkAll("BigInteger", monoid.laws[BigInteger])
  checkAll("BigInt @@ Multiplication", monoid.laws[BigInt @@ Multiplication])
  checkAll("BigInteger @@ Multiplication", monoid.laws[BigInteger @@ Multiplication])
  checkAll("Short", monoid.laws[Short])
  checkAll("Short @@ Multiplication", monoid.laws[Short @@ Multiplication])
  checkAll("Byte", monoid.laws[Byte])
  checkAll("Byte @@ Multiplication", monoid.laws[Byte @@ Multiplication])
  checkAll("Long", monoid.laws[Long])
  checkAll("Long @@ Multiplication", monoid.laws[Long @@ Multiplication])
  //    checkAll("ZipStream[A", monoid.laws[ZipStream[A])]
  checkAll("(A)", monoid.laws[(A)])
  checkAll("(A, B)", monoid.laws[(A, B)])
  checkAll("(A, B, C)", monoid.laws[(A, B, C)])
  checkAll("(A, B, C, D)", monoid.laws[(A, B, C, D)])
  checkAll("(A, B, C, D, E)", monoid.laws[(A, B, C, D, E)])
  checkAll("(A, B, C, D, E, F)", monoid.laws[(A, B, C, D, E, F)])
  checkAll("(A, B, C, D, E, F, G)", monoid.laws[(A, B, C, D, E, F, G)])
  checkAll("(A, B, C, D, E, F, G, H)", monoid.laws[(A, B, C, D, E, F, G, H)])
  checkAll("List[A", monoid.laws[List[A]])
  checkAll("Option", monoid.laws[Option[A]])
  checkAll("Option", monoid.laws[Option[A] @@ First])
  checkAll("Option", monoid.laws[Option[A] @@ Last])
  //    checkAll("ArraySeq[A", monoid.laws[ArraySeq[A])]
  checkAll("Either.LeftProjection", monoid.laws[Either.LeftProjection[A, B]])
  checkAll("Either.LeftProjection @@ First", monoid.laws[Either.LeftProjection[A, B] @@ First])
  checkAll("Either.LeftProjection @@ Last", monoid.laws[Either.LeftProjection[A, B] @@ Last])
  checkAll("Either.RightProjection", monoid.laws[Either.RightProjection[B, A]])
  checkAll("Either.RightProjection @@ First", monoid.laws[Either.RightProjection[B, A] @@ First])
  checkAll("Either.RightProjection @@ Last", monoid.laws[Either.RightProjection[B, A] @@ Last])
  // todo more types
  //    checkAll("Map[A, B", monoid.laws[Map[A, B])]

}
