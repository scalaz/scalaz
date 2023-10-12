package scalaz

object Dual extends DualInstances {
  def apply[A](a: A): (A @@ Tags.Dual) = Tag(a)
}

sealed abstract class DualInstances0 {
  implicit def dualSemigroup[F](implicit F0: Semigroup[F]): Semigroup[F @@ Tags.Dual] = new DualSemigroup[F] {
    override def F = F0
  }
}

sealed abstract class DualInstances extends DualInstances0 {
  implicit def dualMonoid[F](implicit F0: Monoid[F]): Monoid[F @@ Tags.Dual] = new DualMonoid[F] {
    override def F = F0
  }

  implicit def dualOrder[F](implicit F0: Order[F]): Order[F @@ Tags.Dual] =
    Tag subst F0.reverseOrder
}

private trait DualSemigroup[F] extends Semigroup[F @@ Tags.Dual] {
  implicit def F: Semigroup[F]
  def append(f1: F @@ Tags.Dual, f2: => F @@ Tags.Dual) = Tag(F.append(Tag.unwrap(f2), Tag.unwrap(f1)))
}

private trait DualMonoid[F] extends Monoid[F @@ Tags.Dual] with DualSemigroup[F] {
  implicit def F: Monoid[F]
  def zero = Tag(F.zero)
}
