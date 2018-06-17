package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 *
 */
////
trait Derives[F[_]] extends CoapplicativeDecidable[F] with ApplicativeDivisible[F] { self =>
  ////

  ////
  val derivesSyntax = new scalaz.syntax.DerivesSyntax[F] { def F = Derives.this }
}

object Derives {
  @inline def apply[F[_]](implicit F: Derives[F]): Derives[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Derives[G]): Derives[F] =
    new IsomorphismDerives[F, G] {
      override def G: Derives[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismDerives[F[_], G[_]] extends Derives[F] with IsomorphismCoapplicativeDecidable[F, G] with IsomorphismApplicativeDivisible[F, G]{
  implicit def G: Derives[G]
  ////

  ////
}
