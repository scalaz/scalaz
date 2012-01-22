package scalaz
package syntax
package effect

import scalaz.effect.MonadIO

/** Wraps a value `self` and provides methods related to `MonadIO` */
trait MonadIOV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: MonadIO[F]
  ////

  ////
}

trait ToMonadIOV0 {
  implicit def ToMonadIOVUnapply[FA](v: FA)(implicit F0: Unapply[MonadIO, FA]) =
    new MonadIOV[F0.M,F0.A] { def self = F0(v); implicit def F: MonadIO[F0.M] = F0.TC }

}

trait ToMonadIOV extends ToMonadIOV0 with ToLiftIOV with ToMonadV {
  implicit def ToMonadIOV[F[_],A](v: F[A])(implicit F0: MonadIO[F]) =
    new MonadIOV[F,A] { def self = v; implicit def F: MonadIO[F] = F0 }

  ////

  ////
}

trait MonadIOSyntax[F[_]] extends LiftIOSyntax[F] with MonadSyntax[F] {
  implicit def ToMonadIOV[A](v: F[A])(implicit F0: MonadIO[F]): MonadIOV[F, A] = new MonadIOV[F,A] { def self = v; implicit def F: MonadIO[F] = F0 }

  ////

  ////
}
