package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Functor` */
sealed abstract class FunctorOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Functor[F]
  ////
  import Leibniz.===

  final def map[B](f: A => B): F[B] = F.map(self)(f)
  final def distribute[G[_], B](f: A => G[B])(implicit D: Distributive[G]): G[F[B]] = D.distribute(self)(f)
  final def cosequence[G[_], B](implicit ev: A === G[B], D: Distributive[G]): G[F[B]] = D.distribute(self)(ev(_))
  final def cotraverse[G[_], B, C](f: F[B] => C)(implicit ev: A === G[B], D: Distributive[G]): G[C] = D.map(cosequence)(f)
  final def ∘[B](f: A => B): F[B] = F.map(self)(f)
  final def strengthL[B](b: B): F[(B, A)] = F.strengthL(b, self)
  final def strengthR[B](b: B): F[(A, B)] = F.strengthR(self, b)
  final def fpair: F[(A, A)] = F.fpair(self)
  final def fproduct[B](f: A => B): F[(A, B)] = F.fproduct(self)(f)
  final def void: F[Unit] = F.void(self)
  final def fpoint[G[_]: Applicative]: F[G[A]] = F.map(self)(a => Applicative[G].point(a))
  final def >|[B](b: => B): F[B] = F.map(self)(_ => b)
  final def as[B](b: => B): F[B] = F.map(self)(_ => b)
  ////
}

trait ToFunctorOps0 {
  implicit def ToFunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply[Functor, FA]) =
    new FunctorOps[F0.M,F0.A] { def self = F0(v); implicit def F: Functor[F0.M] = F0.TC }

}

trait ToFunctorOps extends ToFunctorOps0 with ToInvariantFunctorOps {
  implicit def ToFunctorOps[F[_],A](v: F[A])(implicit F0: Functor[F]) =
    new FunctorOps[F,A] { def self = v; implicit def F: Functor[F] = F0 }

  ////

  implicit def ToLiftV[F[_], A, B](v: A => B) = new LiftV[F, A, B] { def self = v }

  // TODO Duplication
  trait LiftV[F[_], A, B] extends Ops[A => B] {
    def lift(implicit F: Functor[F]) = F.lift(self)
  }

  implicit def ToFunctorIdV[A](v: A) = new FunctorIdV[A] { def self = v }

  trait FunctorIdV[A] extends Ops[A] {
    def mapply[F[_], B](f: F[A => B])(implicit F: Functor[F]): F[B] =
      F.map(f)(fab => fab(self))
  }
  ////
}

trait FunctorSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToFunctorOps[A](v: F[A]): FunctorOps[F, A] = new FunctorOps[F,A] { def self = v; implicit def F: Functor[F] = FunctorSyntax.this.F }

  def F: Functor[F]
  ////
  implicit def ToLiftV[A, B](v: A => B): LiftV[A, B] = new LiftV[A, B] {
    def self = v
  }

  trait LiftV[A,B] extends Ops[A => B] {
    def lift = F.lift(self)
  }
  ////
}
