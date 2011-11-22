package scalaz

object Dual extends DualInstances {
  def apply[A](a: A): (A @@ Tags.Dual) = Tag(a)
}

trait DualInstances0 {
  implicit def dualSemigroup[F](implicit F0: Semigroup[F]) = new DualSemigroup[F] {
    implicit def F = F0
  }
}

trait DualInstances {
  implicit def dualMonoid[F](implicit F0: Monoid[F]) = new DualMonoid[F] {
    implicit def F = F0
  }
}

trait DualSemigroup[F] extends Semigroup[F @@ Tags.Dual] {
  implicit def F: Semigroup[F]
  def append(f1: F @@ Tags.Dual, f2: => F @@ Tags.Dual) = Tag(F.append(f2, f1))
}

trait DualMonoid[F] extends Monoid[F @@ Tags.Dual] with DualSemigroup[F] {
  implicit def F: Monoid[F]
  def zero = Tag(F.zero)
}