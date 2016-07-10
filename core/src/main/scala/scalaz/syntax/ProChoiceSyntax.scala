package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ProChoice` */
final class ProChoiceOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: ProChoice[F]) extends Ops[F[A, B]] {
  ////
  final def proleft[C]: F[A \/ C, B \/ C] =
    F.left(self)

  final def proright[C]: F[C \/ A, C \/ B] =
    F.right(self)

  ////
}

sealed trait ToProChoiceOps0 {
  implicit def ToProChoiceOpsUnapply[FA](v: FA)(implicit F0: Unapply2[ProChoice, FA]) =
    new ProChoiceOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToProChoiceOps extends ToProChoiceOps0 with ToProfunctorOps {

  implicit def ToProChoiceOps[F[_, _],A, B](v: F[A, B])(implicit F0: ProChoice[F]) =
    new ProChoiceOps[F,A, B](v)


  implicit def ToProChoiceVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: ProChoice[F[G, ?, ?]]) =
    new ProChoiceOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ProChoiceSyntax[F[_, _]] extends ProfunctorSyntax[F] {
  implicit def ToProChoiceOps[A, B](v: F[A, B]): ProChoiceOps[F, A, B] = new ProChoiceOps[F, A, B](v)(ProChoiceSyntax.this.F)

  def F: ProChoice[F]
  ////

  ////
}
