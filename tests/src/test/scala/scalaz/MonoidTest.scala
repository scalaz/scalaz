package scalaz

import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck


class MonoidTest extends Specification with ScalaCheck {
  addArguments(fullStackTrace)
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._
  import Tags._
  import std.boolean._
  implicit val booleanMonoid = std.anyVal.booleanInstance.conjunction
  import std.java.math.bigInteger._
  import std.option._
  import std.either._

  "function monoid" in {
    val f: (Int) => Int = implicitly[Semigroup[Int => Int]].append(_ + 1, _ * 2)
    f(1) must_== 4
  }

  "monoid laws" in {
    type A = Int
    type B = Int
    type C = Int
    type D = Int
    type E = Int
    type F = Int
    type G = Int
    type H = Int

    checkMonoidLaws[Boolean @@ Conjunction]
    checkMonoidLaws[Boolean]
    // todo
    // checkMonoidLaws[Digit]
    checkMonoidLaws[Int]
    checkMonoidLaws[String]
    checkMonoidLaws[List[Int]]
    checkMonoidLaws[BigInt]
    checkMonoidLaws[BigInteger]
    checkMonoidLaws[BigInt @@ Multiplication]
    checkMonoidLaws[BigInteger @@ Multiplication]
    checkMonoidLaws[Short]
    checkMonoidLaws[Short @@ Multiplication]
    checkMonoidLaws[Byte]
    checkMonoidLaws[Byte @@ Multiplication]
    checkMonoidLaws[Long]
    checkMonoidLaws[Long @@ Multiplication]
//    checkMonoidLaws[ZipStream[A]]
    checkMonoidLaws[(A)]
    checkMonoidLaws[(A, B)]
    checkMonoidLaws[(A, B, C)]
    checkMonoidLaws[(A, B, C, D)]
    checkMonoidLaws[(A, B, C, D, E)]
    checkMonoidLaws[(A, B, C, D, E, F)]
    checkMonoidLaws[(A, B, C, D, E, F, G)]
    checkMonoidLaws[(A, B, C, D, E, F, G, H)]
    checkMonoidLaws[List[A]]
    checkMonoidLaws[Option[A]]
    checkMonoidLaws[Option[A] @@ First]
    checkMonoidLaws[Option[A] @@ Last]
//    checkMonoidLaws[ArraySeq[A]]
    checkMonoidLaws[Either.LeftProjection[A, B]]
    checkMonoidLaws[Either.LeftProjection[A, B] @@ First]
    checkMonoidLaws[Either.LeftProjection[A, B] @@ Last]
    checkMonoidLaws[Either.RightProjection[B, A]]
    checkMonoidLaws[Either.RightProjection[B, A] @@ First]
    checkMonoidLaws[Either.RightProjection[B, A] @@ Last]
    // todo more types
//    checkMonoidLaws[Map[A, B]]
  }

  def checkMonoidLaws[A: Monoid : Equal : Manifest : Arbitrary] = {
    val typeName = manifest[A].toString
    typeName should {
      import ScalazProperties.semigroup._
      import ScalazProperties.monoid._

      "associative" in check(associative[A])
      "identity" in check(identity[A])
    }
  }
}
