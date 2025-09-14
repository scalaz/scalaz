package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Functor` */
final class FunctorOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Functor[F]) extends Ops[F[A]] {
  ////
  import Liskov.<~<

  final def map[B](f: A => B): F[B] = F.map(self)(f)
  final def distribute[G[_], B](f: A => G[B])(implicit D: Distributive[G]): G[F[B]] = D.distribute(self)(f)
  final def cosequence[G[_], B](implicit ev: A === G[B], D: Distributive[G]): G[F[B]] = D.distribute(self)(ev)
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
  final def widen[B](implicit ev: A <~< B): F[B] = F.widen(self)
  ////
}

sealed trait ToFunctorOpsU[TC[F[_]] <: Functor[F]] {
  implicit def ToFunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): FunctorOps[F0.M, F0.A] =
    new FunctorOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToFunctorOps0[TC[F[_]] <: Functor[F]] extends ToFunctorOpsU[TC] {
  implicit def ToFunctorOps[F[_],A](v: F[A])(implicit F0: TC[F]): FunctorOps[F, A] =
    new FunctorOps[F, A](v)

  ////

  implicit def ToLiftV[F[_], A, B](v: A => B): LiftV[F, A, B] = new LiftV[F, A, B] { def self = v }

  // TODO Duplication
  trait LiftV[F[_], A, B] extends Ops[A => B] {
    def lift(implicit F: TC[F]): F[A] => F[B] = F.lift(self)
  }

  implicit def ToFunctorIdV[A](v: A): FunctorIdV[A] = new FunctorIdV[A] { def self = v }

  trait FunctorIdV[A] extends Ops[A] {
    def mapply[F[_], B](f: F[A => B])(implicit F: TC[F]): F[B] =
      F.map(f)(fab => fab(self))
  }
  ////
}

trait ToFunctorOps[TC[F[_]] <: Functor[F]] extends ToFunctorOps0[TC] with ToInvariantFunctorOps[TC]

trait FunctorSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToFunctorOps[A](v: F[A]): FunctorOps[F, A] = new FunctorOps[F,A](v)(using FunctorSyntax.this.F)

  def F: Functor[F]
  ////
  implicit def ToLiftV[A, B](v: A => B): LiftV[A, B] = new LiftV[A, B] {
    def self = v
  }

  trait LiftV[A,B] extends Ops[A => B] {
    def lift: F[A] => F[B] = F.lift(self)
  }
  ////
}
