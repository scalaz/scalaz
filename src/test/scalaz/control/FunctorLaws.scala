package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object FunctorLaws {
  def identity[F[_], A](implicit f: Functor[F], af: Arbitrary[F[A]], e: Equal[F[A]]) =
    prop((fa: F[A]) => f.fmap((a: A) => a, fa) === fa)

  def composition[F[_], A, B, C](implicit f: Functor[F],
                                          af: Arbitrary[F[A]],
                                          abc: Arbitrary[B => C],
                                          aab: Arbitrary[A => B],
                                          e: Equal[F[C]]) =
    prop((fa: F[A], h: B => C, i: A => B) =>
            f.fmap(h compose i, fa) === f.fmap(h, f.fmap(i, fa)))
}
