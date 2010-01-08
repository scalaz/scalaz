package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck.Arbitrary
import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}

class MonadTest extends Specification with Sugar with ScalaCheck {
  import Scalaz._
  import ScalaCheckBinding._
  import ScalazArbitrary._

  "monad laws" should {
    type A = Int
    type B = Int

    implicit def IdentityEqual[X] = equalA[Identity[X]]
    checkMonadLaws[Identity, A]
    checkMonadLaws[List, A]
    // todo fix arbitrary instance for Stream
//    checkMonadLaws[Stream, A]
    checkMonadLaws[NonEmptyList, A]
  }

  def checkMonadLaws[M[_], A](implicit mm: Monad[M],
                              ea: Equal[A],
                              man: Manifest[M[A]],
                              ema: Equal[M[A]],
                              arbma: Arbitrary[M[A]],
                              arba: Arbitrary[A]
                              ): Unit = {
    val typeName = man.toString
    typeName in {
      import ScalazProperties.Monad._
      identity[M, A] must pass
      unit[M, A, A] must pass
      composition[M, A, A, A] must pass
    }
  }
}
