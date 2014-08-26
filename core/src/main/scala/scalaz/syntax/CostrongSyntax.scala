package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Costrong` */
final class CostrongOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Costrong[F]) extends Ops[F[A, B]] {
  ////
  final def cofirst[C]: F[A \/ C, B \/ C] =
    F.left(self)

  final def cosecond[C]: F[C \/ A, C \/ B] =
    F.right(self)

  ////
}

sealed trait ToCostrongOps0 {
    implicit def ToCostrongOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Costrong, FA]) =
      new CostrongOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)
  
}

trait ToCostrongOps extends ToCostrongOps0 with ToProfunctorOps {
  
  implicit def ToCostrongOps[F[_, _],A, B](v: F[A, B])(implicit F0: Costrong[F]) =
      new CostrongOps[F,A, B](v)
  

  
  implicit def ToCostrongVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Costrong[({type λ[α, β]=F[G, α, β]})#λ]) =
        new CostrongOps[({type λ[α, β]=F[G, α, β]})#λ, A, B](v)(F0)

  ////

  ////
}

trait CostrongSyntax[F[_, _]] extends ProfunctorSyntax[F] {
  implicit def ToCostrongOps[A, B](v: F[A, B]): CostrongOps[F, A, B] = new CostrongOps[F, A, B](v)(CostrongSyntax.this.F)

  def F: Costrong[F]
  ////

  ////
}
