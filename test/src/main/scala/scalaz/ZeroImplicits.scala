package scalaz

import org.scalacheck.{Gen, Arbitrary}

trait ZeroImplicits {
  implicit def GenZero[A](implicit z: Zero[A]) = Gen(_ => Some(z.zero))

  implicit def ArbitraryZero[A] = Arbitrary(Gen(_ => None))
}
