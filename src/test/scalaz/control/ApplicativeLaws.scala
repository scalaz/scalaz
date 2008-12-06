package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object ApplicativeLaws {
  def identity[A[_], X](implicit a: Applicative[A],
                                 aax: Arbitrary[A[X]],
                                 e: Equal[A[X]]) = 
    prop((apx: A[X]) => apx === a(a.pure((x: X) => x), apx))

  def composition[A[_], X, Y, Z](implicit a: Applicative[A],
                                aayz: Arbitrary[A[Y => Z]],
                                aaxy: Arbitrary[A[X => Y]],
                                aax: Arbitrary[A[X]],
                                e: Equal[A[Z]]) =
    prop((apyz: A[Y => Z], apxy: A[X => Y], apx: A[X]) =>
            a(apyz, a(apxy, apx)) ===
            a(a(a(a.pure((f: Y => Z) => (g: X => Y) => f compose g), apyz), apxy), apx))

  def homomorphism[A[_], X, Y](implicit a: Applicative[A],
                                        ax: Arbitrary[X],
                                        axy: Arbitrary[X => Y],
                                        e: Equal[A[Y]]) =
    prop((f: X => Y, x: X) => a(a.pure(f), a.pure(x)) === a.pure(f(x)))

  def interchange[A[_], X, Y](implicit a: Applicative[A],
                                       ax: Arbitrary[X],
                                       apxy: Arbitrary[A[X => Y]],
                                       e: Equal[A[Y]]) =
    prop((f: A[X => Y], x: X) =>
      a(f, a.pure(x)) === a(a.pure((f: X => Y) => f(x)), f))
}
