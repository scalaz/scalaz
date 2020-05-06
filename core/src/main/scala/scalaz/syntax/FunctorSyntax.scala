package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Functor` */
final class FunctorOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Functor[F]) extends Ops[F[A]] {
  ////
  import Leibniz.===
  import Liskov.<~<

  final def map[B](f: A => B): F[B] = F.map(self)(f)

  /**
    * Alias for [[map]], since [[map]] can't be injected as syntax if
    * the implementing type already had a built-in `.map` method.
    *
    * Example:
    * {{{
    * scala> import scalaz.Scalaz._
    *
    * scala> val m: Map[Int, String] = Map(1 -> "hi", 2 -> "there", 3 -> "you")
    *
    * scala> m.fmap(_ ++ "!")
    * res0: Map[Int,String] = Map(1 -> hi!, 2 -> there!, 3 -> you!)
    * }}}
    */
  final def fmap[B](f: A => B): F[B] = F.map(self)(f)

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

sealed trait ToFunctorOps0 {
  implicit def ToFunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply[Functor, FA]): FunctorOps[F0.M, F0.A] =
    new FunctorOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToFunctorOps extends ToFunctorOps0 with ToInvariantFunctorOps {
  implicit def ToFunctorOps[F[_], A](v: F[A])(implicit F0: Functor[F]): FunctorOps[F, A] =
    new FunctorOps[F, A](v)

  ////

  implicit def ToLiftV[F[_], A, B](v: A => B): LiftV[F, A, B] = new LiftV[F, A, B] { def self = v }

  // TODO Duplication
  trait LiftV[F[_], A, B] extends Ops[A => B] {
    def lift(implicit F: Functor[F]): F[A] => F[B] = F.lift(self)
  }

  implicit def ToFunctorIdV[A](v: A): FunctorIdV[A] = new FunctorIdV[A] { def self = v }

  trait FunctorIdV[A] extends Ops[A] {
    def mapply[F[_], B](f: F[A => B])(implicit F: Functor[F]): F[B] =
      F.map(f)(fab => fab(self))
  }
  ////
}

trait FunctorSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToFunctorOps[A](v: F[A]): FunctorOps[F, A] = new FunctorOps[F,A](v)(FunctorSyntax.this.F)

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
