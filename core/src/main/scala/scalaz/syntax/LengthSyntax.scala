package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Length` */
sealed abstract class LengthOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Length[F]
  ////
  @deprecated("Length#length is deprecated, use Foldable#length instead", "7.1")
  final def length: Int = F.length(self)
  ////
}

trait ToLengthOps0 {
  implicit def ToLengthOpsUnapply[FA](v: FA)(implicit F0: Unapply[Length, FA]) =
    new LengthOps[F0.M,F0.A] { def self = F0(v); implicit def F: Length[F0.M] = F0.TC }

}

trait ToLengthOps extends ToLengthOps0 {
  implicit def ToLengthOps[F[_],A](v: F[A])(implicit F0: Length[F]) =
    new LengthOps[F,A] { def self = v; implicit def F: Length[F] = F0 }

  ////

  ////
}

trait LengthSyntax[F[_]]  {
  implicit def ToLengthOps[A](v: F[A]): LengthOps[F, A] = new LengthOps[F,A] { def self = v; implicit def F: Length[F] = LengthSyntax.this.F }

  def F: Length[F]
  ////

  ////
}
