package scalaz
package syntax

import scalaz.{Foldable, Cofoldable}

final class CofoldableOps[F[_], A](val self: F[A])(implicit val F: Cofoldable[F, A]) extends Ops[F[A]] {
  ////

  final def filterAll(f: A => Boolean)(implicit G: Foldable[F]): F[A] =
    F.filterAll(self, f)

  final def collectall[B](pf: PartialFunction[A, A])(implicit G: Foldable[F]): F[A] =
    F.collectall(self, pf)

  final def splitup(f: A => Boolean)(implicit G: Foldable[F]): (F[A], F[A]) = F.splitup(self, f)

  ////
}

trait ToCofoldableOps[TC[F[_], A] <: Cofoldable[F, A]] {
  implicit def ToCofoldableOps[F[_], A](v: F[A])(implicit F0: TC[F, A]) =
    new CofoldableOps[F, A](v)
}

trait CofoldableSyntax[F[_], A] {
  implicit def ToCofoldableOps(fa: F[A]): CofoldableOps[F, A] = new CofoldableOps[F, A](fa)(CofoldableSyntax.this.F)

  def F: Cofoldable[F, A]
}
