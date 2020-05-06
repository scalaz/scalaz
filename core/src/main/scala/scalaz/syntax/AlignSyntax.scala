package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Align` */
final class AlignOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Align[F]) extends Ops[F[A]] {
  ////
  def align[B](b: F[B]): F[A \&/ B] = F.align(self, b)
  def alignWith[B, C](fb: F[B])(f: (A \&/ B) => C): F[C] = F.alignWith(f)(self, fb)
  def pad[B](b: F[B]): F[(Option[A], Option[B])] = F.pad(self, b)
  def padWith[B, C](fb: F[B])(f: (Option[A], Option[B]) => C): F[C] = F.padWith(f)(self, fb)
  def merge(a2: F[A])(implicit S: Semigroup[A]): F[A] = F.merge(self, a2)
  def alignSwap[B](b: F[B]): F[B \&/ A] = F.alignSwap(self, b)
  def alignA[B](b: F[B]): F[Option[A]] = F.alignA(self, b)
  def alignB[B](b: F[B]): F[Option[B]] = F.alignB(self, b)
  def alignThis[B](b: F[B]): F[Option[A]] = F.alignThis(self, b)
  def alignThat[B](b: F[B]): F[Option[B]] = F.alignThat(self, b)
  def alignBoth[B](b: F[B]): F[Option[(A, B)]] = F.alignBoth(self, b)
  ////
}

sealed trait ToAlignOps0 {
  implicit def ToAlignOpsUnapply[FA](v: FA)(implicit F0: Unapply[Align, FA]): AlignOps[F0.M, F0.A] =
    new AlignOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToAlignOps extends ToAlignOps0 with ToFunctorOps {
  implicit def ToAlignOps[F[_], A](v: F[A])(implicit F0: Align[F]): AlignOps[F, A] =
    new AlignOps[F, A](v)

  ////

  ////
}

trait AlignSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToAlignOps[A](v: F[A]): AlignOps[F, A] = new AlignOps[F,A](v)(AlignSyntax.this.F)

  def F: Align[F]
  ////

  ////
}
