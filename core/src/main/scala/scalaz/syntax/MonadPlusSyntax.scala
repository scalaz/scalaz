package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
final class MonadPlusOps[F[_],A] private[syntax](val self: F[A])(implicit val F: MonadPlus[F]) extends Ops[F[A]] {
  ////

  def filter(f: A => Boolean): F[A] =
    F.filter(self)(f)

  def withFilter(f: A => Boolean): F[A] =
    filter(f)

  final def uniteU[T](implicit T: Unapply[Foldable, A]): F[T.A] =
    F.uniteU(self)(T)

  def unite[T[_], B](implicit ev: A === T[B], T: Foldable[T]): F[B] = {
    val ftb: F[T[B]] = ev.subst(self)
    F.unite[T, B](ftb)
  }
  final def lefts[G[_, _], B, C](implicit ev: A === G[B, C], G: Bifoldable[G]): F[B] =
    F.lefts(ev.subst(self))

  final def rights[G[_, _], B, C](implicit ev: A === G[B, C], G: Bifoldable[G]): F[C] =
    F.rights(ev.subst(self))

  final def separate[G[_, _], B, C](implicit ev: A === G[B, C], G: Bifoldable[G]): (F[B], F[C]) =
    F.separate(ev.subst(self))

  ////
}

sealed trait ToMonadPlusOpsU[TC[F[_]] <: MonadPlus[F]] {
  implicit def ToMonadPlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): MonadPlusOps[F0.M, F0.A] =
    new MonadPlusOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToMonadPlusOps0[TC[F[_]] <: MonadPlus[F]] extends ToMonadPlusOpsU[TC] {
  implicit def ToMonadPlusOps[F[_],A](v: F[A])(implicit F0: TC[F]): MonadPlusOps[F, A] =
    new MonadPlusOps[F, A](v)

  ////

  ////
}

trait ToMonadPlusOps[TC[F[_]] <: MonadPlus[F]] extends ToMonadPlusOps0[TC] with ToMonadOps[TC] with ToApplicativePlusOps[TC]

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusOps[A](v: F[A]): MonadPlusOps[F, A] = new MonadPlusOps[F,A](v)(using MonadPlusSyntax.this.F)

  def F: MonadPlus[F]
  ////

  ////
}
