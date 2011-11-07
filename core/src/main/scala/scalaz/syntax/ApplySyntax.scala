package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
trait ApplyV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Apply[F]
  ////
  final def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  ////
}

trait ToApplyV extends ToFunctorV {
  implicit def ToApplyV[FA](v: FA)(implicit F0: Unapply[Apply, FA]) =
    new ApplyV[F0.M,F0.A] { def self = F0(v); implicit def F: Apply[F0.M] = F0.TC }

  ////

  ////
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToApplyV[A](v: F[A])(implicit F0: Apply[F]): ApplyV[F, A] = new ApplyV[F,A] { def self = v; implicit def F: Apply[F] = F0 }

  ////

  ////
}
