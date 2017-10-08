package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Choice` */
final class ChoiceOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Choice[F]) extends Ops[F[A, B]] {
  ////
  final def |||[C](x: => F[C, B]): F[A \/ C, B] =
    F.choice(self, x)
  ////
}

sealed trait ToChoiceOpsU[TC[F[_, _]] <: Choice[F]] {
  implicit def ToChoiceOpsUnapply[FA](v: FA)(implicit F0: Unapply2[TC, FA]) =
    new ChoiceOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToChoiceOps0[TC[F[_, _]] <: Choice[F]] extends ToChoiceOpsU[TC] {

  implicit def ToChoiceOps[F[_, _],A, B](v: F[A, B])(implicit F0: TC[F]) =
    new ChoiceOps[F,A, B](v)


  implicit def ToChoiceVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: TC[F[G, ?, ?]]) =
    new ChoiceOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ToChoiceOps[TC[F[_, _]] <: Choice[F]] extends ToChoiceOps0[TC] with ToCategoryOps[TC]

trait ChoiceSyntax[F[_, _]] extends CategorySyntax[F] {
  implicit def ToChoiceOps[A, B](v: F[A, B]): ChoiceOps[F, A, B] = new ChoiceOps[F, A, B](v)(ChoiceSyntax.this.F)

  def F: Choice[F]
  ////

  ////
}
