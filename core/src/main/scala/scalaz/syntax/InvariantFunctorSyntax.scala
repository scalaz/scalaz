package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `InvariantFunctor`. */
trait InvariantFunctorOps[F[_], A] extends Ops[F[A]] {
  implicit def F: InvariantFunctor[F]
  ////

  import BijectionT.Bijection
  import Isomorphism.<=>

  final def xmap[B](f: A => B, g: B => A): F[B] = F.xmap(self, f, g)
  final def xmapb[B](b: Bijection[A, B]): F[B] = F.xmapb(self)(b)
  final def xmapi[B](iso: A <=> B): F[B] = F.xmapi(self)(iso)
  ////
}

trait ToInvariantFunctorOps0 {
  implicit def ToInvariantFunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply[InvariantFunctor, FA]) =
    new InvariantFunctorOps[F0.M, F0.A] {
      def self = F0(v)
      implicit def F: InvariantFunctor[F0.M] = F0.TC
    }
}

trait ToInvariantFunctorOps extends ToInvariantFunctorOps0 {
  implicit def ToInvariantFunctorOps[F[_], A](v: F[A])(implicit F0: InvariantFunctor[F]) =
    new InvariantFunctorOps[F, A] {
      def self = v
      implicit def F: InvariantFunctor[F] = F0
    }

  ////

  ////
}

trait InvariantFunctorSyntax[F[_]]  {
  implicit def ToInvariantFunctorOps[A](v: F[A]): InvariantFunctorOps[F, A] =
    new InvariantFunctorOps[F,A] {
      def self = v
      implicit def F: InvariantFunctor[F] = InvariantFunctorSyntax.this.F
    }

  def F: InvariantFunctor[F]
  ////

  ////
}
