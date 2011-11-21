package scalaz

import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}
import org.scalacheck.{Gen, Arbitrary}
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class ApplicativeTest extends Specification with ScalaCheck {

  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._
  import std.option._
  import syntax.monad._

  "applicative laws" should {
    type A = Int
    type B = Int
    type C = Int
    type D = Int
    type E = Int
    type F = Int
    type G = Int
    type R = Int
    type X = Int
    type Z = Int

    checkApplicativeLaws[Option, A]
    checkApplicativeLaws[Zipper, A]

    ok
  }

  def checkApplicativeLaws[M[_], A](implicit mm: Applicative[M],
                                    ea: Equal[A],
                                    man: Manifest[M[A]],
                                    ema: Equal[M[A]],
                                    arbma: Arbitrary[M[A]],
                                    arba: Arbitrary[A]) = {
    val typeName = man.toString
    implicit val arbMAA: Arbitrary[M[A => A]] = ((a: A) => a).point[M].point[Arbitrary]
    typeName should {
      import ScalazProperties.applicative._

      "identity" in check(identity[M, A])
      "composition" in check(composition[M, A, A, A])
      
      // TODO These don't terminate. Investigate.
//      homomorphism[M, A, A] must pass
//      interchange[M, A, A] must pass
    }
  }
}
