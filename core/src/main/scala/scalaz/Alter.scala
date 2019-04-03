package scalaz

/** Derive a Semigroup or Monoid instance from a Plus or PlusEmpty.
 *
 * [[https://hackage.haskell.org/package/reducers-3.12.1/docs/Data-Semigroup-Alt.html#t:Alter]]
 */
final case class Alter[F[_], A](f: F[A])

object Alter extends AlterInstances

sealed abstract class AlterInstances0 {
  implicit def AlterSemigroup[F[_], A](implicit F0: Plus[F]): Semigroup[Alter[F, A]] =
    new AlterSemigroup[F, A] {
      val F = F0
    }
}

sealed abstract class AlterInstances extends AlterInstances0 {
  implicit def AlterMonoid[F[_], A](implicit F0: PlusEmpty[F]): Monoid[Alter[F, A]] =
    new AlterMonoid[F, A] {
      val F = F0
    }

  implicit def AlterEqual[F[_], A](implicit E: Equal[F[A]]): Equal[Alter[F, A]] =
    E.contramap(_.f)
}

private sealed trait AlterSemigroup[F[_], A] extends Semigroup[Alter[F, A]] {
  def F: Plus[F]

  def append(f1: Alter[F, A], f2: => Alter[F, A]): Alter[F, A] =
    Alter(F.plus(f1.f, f2.f))
}

private sealed trait AlterMonoid[F[_], A] extends AlterSemigroup[F, A] with Monoid[Alter[F, A]] {
  def F: PlusEmpty[F]

  def zero: Alter[F, A] =
    Alter(F.empty)
}
