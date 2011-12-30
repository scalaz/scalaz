package scalaz
package syntax
package effect

import scalaz.effect.LiftControlIO

/** Wraps a value `self` and provides methods related to `LiftControlIO` */
trait LiftControlIOV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: LiftControlIO[F]
  ////

  ////
}

trait ToLiftControlIOV0 {
  implicit def ToLiftControlIOVUnapply[FA](v: FA)(implicit F0: Unapply[LiftControlIO, FA]) =
    new LiftControlIOV[F0.M,F0.A] { def self = F0(v); implicit def F: LiftControlIO[F0.M] = F0.TC }

}

trait ToLiftControlIOV extends ToLiftControlIOV0 {
  implicit def ToLiftControlIOV[F[_],A](v: F[A])(implicit F0: LiftControlIO[F]) =
    new LiftControlIOV[F,A] { def self = v; implicit def F: LiftControlIO[F] = F0 }

  ////

  ////
}

trait LiftControlIOSyntax[F[_]]  {
  implicit def ToLiftControlIOV[A](v: F[A])(implicit F0: LiftControlIO[F]): LiftControlIOV[F, A] = new LiftControlIOV[F,A] { def self = v; implicit def F: LiftControlIO[F] = F0 }

  ////

  ////
}
