package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object MonadLaws {
  def functorMonad[M[_], A, B](implicit m: Monad[M], ama: Arbitrary[M[A]], af: Arbitrary[A => B], e: Equal[M[B]]) =
    prop((x: M[A], f: A => B) => m.fmap(f, x) === m.bind((a: A) => m.pure(f(a)), x))

  def leftIdentity[M[_], A, B](implicit m: Monad[M],
                                     am: Arbitrary[A => M[B]],
                                     aa: Arbitrary[A],
                                     e: Equal[M[B]]) =
    prop((a: A, f: A => M[B]) => m.bind(f, m.pure(a)) === f(a))

  def rightIdentity[M[_], A](implicit m: Monad[M],
                                      am: Arbitrary[M[A]],
                                      e: Equal[M[A]]) =
    prop((ma: M[A]) => m.bind(m.pure(_: A), ma) === ma)

  def associativity[M[_], A, B, C](implicit m: Monad[M],
                                           am: Arbitrary[M[A]],
                                           amb: Arbitrary[A => M[B]],
                                           amc: Arbitrary[B => M[C]],
                                           e: Equal[M[C]]) =
    prop((ma: M[A], f: A => M[B], g: B => M[C]) => m.bind(g, m.bind(f, ma)) === m.bind((a: A) => m.bind(g, f(a)), ma))
}
