package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifoldable` */
final class BifoldableOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Bifoldable[F]) extends Ops[F[A, B]] {
  ////

  def bifoldMap[M: Monoid](f: A => M)(g: B => M): M = F.bifoldMap(self)(f)(g)
  def bifoldRight[C](z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C = F.bifoldRight(self, z)(f)(g)
  def bifoldLeft[C](z: C)(f: (C, A) => C)(g: (C, B) => C): C = F.bifoldLeft(self, z)(f)(g)
  def bifoldMap1[M: Semigroup](f: A => M)(g: B => M): Option[M] = F.bifoldMap1(self)(f)(g)
  def bifoldR[C](z: => C)(f: A => (=> C) => C)(g: B => (=> C) => C): C = F.bifoldR(self, z)(f)(g)
  def bifoldL[C](z: C)(f: C => A => C)(g: C => B => C): C = F.bifoldL(self, z)(f)(g)

  ////
}

sealed trait ToBifoldableOps0 {
  implicit def ToBifoldableOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bifoldable, FA]) =
    new BifoldableOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToBifoldableOps extends ToBifoldableOps0 {

  implicit def ToBifoldableOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bifoldable[F]) =
    new BifoldableOps[F,A, B](v)


  implicit def ToBifoldableVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Bifoldable[F[G, ?, ?]]) =
    new BifoldableOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait BifoldableSyntax[F[_, _]]  {
  implicit def ToBifoldableOps[A, B](v: F[A, B]): BifoldableOps[F, A, B] = new BifoldableOps[F, A, B](v)(BifoldableSyntax.this.F)

  def F: Bifoldable[F]
  ////

  ////
}
