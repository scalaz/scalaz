package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Profunctor` */
final class ProfunctorOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Profunctor[F]) extends Ops[F[A, B]] {
  ////

  final def ^>>[C](f: C => A): F[C, B] =
    F.mapfst(self)(f)

  final def >>^[C](f: B => C): F[A, C] =
    F.mapsnd(self)(f)

  final def mapfst[C](f: C => A): F[C, B] =
    F.mapfst(self)(f)

  final def mapsnd[C](f: B => C): F[A, C] =
    F.mapsnd(self)(f)

  final def dimap[C, D](f: C => A, g: B => D): F[C, D] =
    F.dimap(self)(f)(g)

  ////
}

sealed trait ToProfunctorOpsU[TC[F[_, _]] <: Profunctor[F]] {
  implicit def ToProfunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply2[TC, FA]) =
    new ProfunctorOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToProfunctorOps0[TC[F[_, _]] <: Profunctor[F]] extends ToProfunctorOpsU[TC] {

  implicit def ToProfunctorOps[F[_, _],A, B](v: F[A, B])(implicit F0: TC[F]) =
    new ProfunctorOps[F,A, B](v)


  implicit def ToProfunctorVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: TC[F[G, ?, ?]]) =
    new ProfunctorOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ToProfunctorOps[TC[F[_, _]] <: Profunctor[F]] extends ToProfunctorOps0[TC]

trait ProfunctorSyntax[F[_, _]]  {
  implicit def ToProfunctorOps[A, B](v: F[A, B]): ProfunctorOps[F, A, B] = new ProfunctorOps[F, A, B](v)(ProfunctorSyntax.this.F)

  def F: Profunctor[F]
  ////

  ////
}
