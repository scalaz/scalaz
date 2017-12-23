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

  ////

  ////
}
