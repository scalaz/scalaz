package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/** an invariant parent of Applicative / Divisible
 */
////
trait ApplicativeDivisible[F[_]] extends ApplyDivide[F] { self =>
  ////

  def xproduct0[Z](f: =>Z): F[Z]
  final def xderiving0[Z](z: Z): F[Z] = xproduct0(z)

  ////
  val applicativeDivisibleSyntax = new scalaz.syntax.ApplicativeDivisibleSyntax[F] { def F = ApplicativeDivisible.this }
}

object ApplicativeDivisible {
  @inline def apply[F[_]](implicit F: ApplicativeDivisible[F]): ApplicativeDivisible[F] = F

  ////

  ////
}
