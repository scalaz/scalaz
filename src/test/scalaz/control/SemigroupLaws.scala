package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object SemigroupLaws {
  def associative[A](implicit s: Semigroup[A], aa: Arbitrary[A], e: Equal[A]) =
    prop((s1: A, s2: A, s3: A) => s.append(s1, s.append(s2, s3)) === s.append(s.append(s1, s2), s3))
}
