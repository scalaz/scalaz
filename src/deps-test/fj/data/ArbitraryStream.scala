package fj.data

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import ArbitraryList.arbitraryList

object ArbitraryStream {
  implicit def arbitraryStream[A](implicit a: Arbitrary[A]): Arbitrary[Stream[A]] =
    Arbitrary(arbitrary[List[A]].map(_.toStream))
}
