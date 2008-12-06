package scalaz.list

import fjs.test.Arbitrary
import fjs.test.Arbitrary._
import NonEmptyList.nel

object ArbitraryNonEmptyList {
  implicit def ArbitraryNonEmptyList[A](implicit aa: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    aa >>= (arbSList(aa), a => (as: List[A]) => nel(a, as))
}
