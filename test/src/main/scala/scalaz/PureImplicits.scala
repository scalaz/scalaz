package scalaz

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Gen.value
  
trait PureImplicits {
  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A) : Gen[A] = Gen.sized(_ => value(a))
  }

  implicit val ArbitraryPure: Pure[Arbitrary] = new Pure[Arbitrary] {
    def pure[A](a: => A) = Arbitrary(Gen.sized(_ => value(a)))
  }
}
