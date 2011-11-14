package scalaz
package syntax
package effect

import scalaz.effect.LiftIO

/** Wraps a value `self` and provides methods related to `LiftIO` */
trait LiftIOV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: LiftIO[F]
  ////

  ////
}

trait ToLiftIOV  {
  implicit def ToLiftIOV[FA](v: FA)(implicit F0: Unapply[LiftIO, FA]) =
    new LiftIOV[F0.M,F0.A] { def self = F0(v); implicit def F: LiftIO[F0.M] = F0.TC }

  ////

  ////
}

trait LiftIOSyntax[F[_]]  {
  implicit def ToLiftIOV[A](v: F[A])(implicit F0: LiftIO[F]): LiftIOV[F, A] = new LiftIOV[F,A] { def self = v; implicit def F: LiftIO[F] = F0 }

  ////

  ////
}
