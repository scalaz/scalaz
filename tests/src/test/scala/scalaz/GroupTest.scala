package scalaz

import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck


class GroupTest extends Specification with ScalaCheck {
  addArguments(fullStackTrace)
  import Scalaz._
  import ScalazArbitrary._
  import Tags._
  implicit val booleanGroup = std.anyVal.booleanInstance.conjunction
  import std.java.math.bigInteger._
  import std.option._
  import std.either._

  "function minus" in {
    ({ x:Int => x + 1 } |-| { x:Int => x + 2 }).apply(1) must_== -1
  }

  "group laws" in {
    type A = Int
    type B = Int
    type C = Int
    type D = Int
    type E = Int
    type F = Int
    type G = Int
    type H = Int

    checkGroupLaws[Int]
    checkGroupLaws[BigInt]
    checkGroupLaws[BigInteger]
    checkGroupLaws[Short]
    checkGroupLaws[Long]
    checkGroupLaws[(A)]
    checkGroupLaws[(A, B)]
    checkGroupLaws[(A, B, C)]
    checkGroupLaws[(A, B, C, D)]
    checkGroupLaws[(A, B, C, D, E)]
    checkGroupLaws[(A, B, C, D, E, F)]
    checkGroupLaws[(A, B, C, D, E, F, G)]
    checkGroupLaws[(A, B, C, D, E, F, G, H)]

  }

  def checkGroupLaws[A: Group : Equal : Manifest : Arbitrary] = {
    val typeName = manifest[A].toString
    typeName should {
      import ScalazProperties.semigroup._
      import ScalazProperties.monoid._
      import ScalazProperties.group._

      "associative" in check(associative[A])
      "left identity" in check(leftIdentity[A])
      "right identity" in check(rightIdentity[A])
      "inverseExists" in check(inverseExists[A])
    }
  }
}
