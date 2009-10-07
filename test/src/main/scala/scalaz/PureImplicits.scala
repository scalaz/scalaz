package scalaz

import org.scalacheck.{Gen, Arbitrary}

trait PureImplicits {
  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A) = Gen.sized(_ => a)
  }

  implicit val ArbitraryPure: Pure[Arbitrary] = new Pure[Arbitrary] {
    def pure[A](a: => A) = Arbitrary(Gen.sized(_ => a))
  }

}
