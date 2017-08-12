package scalaz

/** Derive a Semigroup or Monoid instance from an Apply or Applicative.
 *
 * [[https://hackage.haskell.org/package/reducers-3.12.1/docs/Data-Semigroup-Applicative.html#t:Ap]]
 */
final case class Ap[F[_], A](f: F[A])

object Ap extends ApInstances

sealed abstract class ApInstances0 {
  implicit def ApSemigroup[F[_], A](implicit F0: Apply[F], A0: Semigroup[A]): Semigroup[Ap[F, A]] =
    new ApSemigroup[F, A] {
      val F = F0
      val A = A0
    }
}

sealed abstract class ApInstances extends ApInstances0 {
  implicit def ApMonoid[F[_], A](implicit F0: Applicative[F], A0: Monoid[A]): Monoid[Ap[F, A]] =
    new ApMonoid[F, A] {
      val F = F0
      val A = A0
    }

  implicit def ApEqual[F[_], A](implicit E: Equal[F[A]]): Equal[Ap[F, A]] =
    E.contramap(_.f)
}

private sealed trait ApSemigroup[F[_], A] extends Semigroup[Ap[F, A]] {
  def F: Apply[F]
  def A: Semigroup[A]

  def append(f1: Ap[F, A], f2: => Ap[F, A]): Ap[F, A] =
    Ap(F.apply2(f1.f, f2.f)(A.append(_, _)))
}

private sealed trait ApMonoid[F[_], A] extends ApSemigroup[F, A] with Monoid[Ap[F, A]] {
  def F: Applicative[F]
  def A: Monoid[A]

  def zero: Ap[F, A] =
    Ap(F.point(A.zero))
}
