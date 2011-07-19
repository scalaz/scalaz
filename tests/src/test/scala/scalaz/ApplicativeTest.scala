package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}
import org.scalacheck.{Gen, Arbitrary}

class ApplicativeTest extends Specification with Sugar with ScalaCheck {

  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._

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

    checkApplicativeLaws[ZipStream, A]
  }

  def checkApplicativeLaws[M[_], A](implicit mm: Applicative[M],
                                    ea: Equal[A],
                                    man: Manifest[M[A]],
                                    ema: Equal[M[A]],
                                    arbma: Arbitrary[M[A]],
                                    arba: Arbitrary[A]): Unit = {
    val typeName = man.toString
    implicit val arbMAA: Arbitrary[M[A => A]] = ((a: A) => a).pure[M].pure[Arbitrary]
    typeName in {
      import ScalazProperties.Applicative._
      identity[M, A] must pass
      composition[M, A, A, A] must pass
      
      // TODO These don't terminate. Investigate.
//      homomorphism[M, A, A] must pass
//      interchange[M, A, A] must pass
    }
  }
}