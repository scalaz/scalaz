package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
trait MonadPlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: MonadPlus[F]
  ////

  ////
}

trait ToMonadPlusV extends ToMonadV with ToApplicativePlusV {
  implicit def ToMonadPlusV[FA](v: FA)(implicit F0: Unapply[MonadPlus, FA]) =
    new MonadPlusV[F0.M,F0.A] { def self = F0(v); implicit def F: MonadPlus[F0.M] = F0.TC }

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusV[A](v: F[A])(implicit F0: MonadPlus[F]): MonadPlusV[F, A] = new MonadPlusV[F,A] { def self = v; implicit def F: MonadPlus[F] = F0 }

  ////

  ////
}
