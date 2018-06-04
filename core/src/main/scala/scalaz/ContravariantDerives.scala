package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 *
 */
////
trait ContravariantDerives[F[_]] extends Derives[F] with Codivide[F] with Divisible[F] { self =>
  ////

  ////
  val contravariantDerivesSyntax = new scalaz.syntax.ContravariantDerivesSyntax[F] { def F = ContravariantDerives.this }
}

object ContravariantDerives {
  @inline def apply[F[_]](implicit F: ContravariantDerives[F]): ContravariantDerives[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: ContravariantDerives[G]): ContravariantDerives[F] =
    new IsomorphismContravariantDerives[F, G] {
      override def G: ContravariantDerives[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismContravariantDerives[F[_], G[_]] extends ContravariantDerives[F] with IsomorphismDerives[F, G] with IsomorphismCodivide[F, G] with IsomorphismDivisible[F, G]{
  implicit def G: ContravariantDerives[G]
  ////

  ////
}
