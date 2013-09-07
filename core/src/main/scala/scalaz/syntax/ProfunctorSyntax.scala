package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Profunctor` */
sealed abstract class ProfunctorOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Profunctor[F]
  ////

  final def ^>>[C](f: C => A): F[C, B] =
    F.mapfst(self)(f)

  final def >>^[C](f: B => C): F[A, C] =
    F.mapsnd(self)(f)

  final def mapfst[C](f: C => A): F[C, B] =
    F.mapfst(self)(f)

  final def mapsnd[C](f: B => C): F[A, C] =
    F.mapsnd(self)(f)

  ////
}

sealed trait ToProfunctorOps0 {
    implicit def ToProfunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Profunctor, FA]) =
      new ProfunctorOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Profunctor[F0.M] = F0.TC }
  
}

trait ToProfunctorOps extends ToProfunctorOps0 {
  
  implicit def ToProfunctorOps[F[_, _],A, B](v: F[A, B])(implicit F0: Profunctor[F]) =
      new ProfunctorOps[F,A, B] { def self = v; implicit def F: Profunctor[F] = F0 }
  

  
  implicit def ToProfunctorVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Profunctor[({type λ[α, β]=F[G, α, β]})#λ]) =
        new ProfunctorOps[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Profunctor[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////

  ////
}

trait ProfunctorSyntax[F[_, _]]  {
  implicit def ToProfunctorOps[A, B](v: F[A, B]): ProfunctorOps[F, A, B] = new ProfunctorOps[F, A, B] { def self = v; implicit def F: Profunctor[F] = ProfunctorSyntax.this.F }

  def F: Profunctor[F]
  ////

  ////
}
