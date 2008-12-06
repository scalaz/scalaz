package fj

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object ArbitraryP {
  implicit def arbitraryP1[A](implicit a: Arbitrary[A]): Arbitrary[P1[A]] =
    Arbitrary(arbitrary[A].map(a => new P1[A]{
      def _1 = a
    }))

}