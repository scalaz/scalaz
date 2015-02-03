package scalaz
package syntax
package effect

import scalaz.effect.Capture
import scalaz.effect.Resource

/** Wraps a value `self` and provides methods related to [[[scalaz.effect.Capture]]]. */
sealed abstract class CaptureOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Capture[F]
  ////
  def unsafeRun: A = F.unsafeRun(self)
  ////
}

sealed trait ToCaptureOps0 {
  implicit def ToCaptureOpsUnapply[FA](v: FA)(implicit F0: Unapply[Capture, FA]) =
    new CaptureOps[F0.M,F0.A] { def self = F0(v); implicit def F: Capture[F0.M] = F0.TC }

}

trait ToCaptureOps extends ToCaptureOps0 {
  implicit def ToCaptureOps[F[_],A](v: F[A])(implicit F0: Capture[F]) =
    new CaptureOps[F,A] { def self = v; implicit def F: Capture[F] = F0 }

  ////

  ////
}

trait CaptureSyntax[F[_]] {
  implicit def ToCaptureOps[A](v: F[A])(implicit F0: Capture[F]): CaptureOps[F, A] = new CaptureOps[F,A] { def self = v; implicit def F: Capture[F] = F0 }

  ////

  ////
}
