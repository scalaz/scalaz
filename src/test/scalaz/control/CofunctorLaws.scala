package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object CofunctorLaws {
  def identity[F[_], A](implicit f: Cofunctor[F], af: Arbitrary[F[A]], e: Equal[F[A]]) =
    prop((fa: F[A]) => f.comap((a: A) => a, fa) === fa)

  def composition[F[_], A, B, C](implicit f: Cofunctor[F],
                                          af: Arbitrary[F[C]],
                                          abc: Arbitrary[B => C],
                                          aab: Arbitrary[A => B],
                                          e: Equal[F[A]]) =
    prop((fc: F[C], h: B => C, i: A => B) => f.comap(h compose i, fc) === f.comap(i, f.comap(h, fc)))
}
