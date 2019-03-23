package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bind` */
final class BindOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Bind[F]) extends Ops[F[A]] {
  ////
  import Liskov.<~<

  def flatMap[B](f: A => F[B]): F[B] = F.bind(self)(f)

  def >>=[B](f: A => F[B]): F[B] = F.bind(self)(f)

  def ∗[B](f: A => F[B]): F[B] = F.bind(self)(f)

  def join[B](implicit ev: A <~< F[B]): F[B] = F.bind(self)(ev)

  def μ[B](implicit ev: A <~< F[B]): F[B] = F.bind(self)(ev)

  def >>[B](b: => F[B]): F[B] = F.bind(self)(_ => b)

  def >>![B](f: A => F[B]): F[A] = F.bind(self)(a => F.map(f(a))(_ => a))

  def ifM[B](ifTrue: => F[B], ifFalse: => F[B])(implicit ev: A === Boolean): F[B] = {
    val value: F[Boolean] = ev.subst(self)
    F.ifM(value, ifTrue, ifFalse)
  }

  def mproduct[B](f: A => F[B]): F[(A, B)] = F.mproduct(self)(f)

  ////
}

sealed trait ToBindOpsU[TC[F[_]] <: Bind[F]] {
  implicit def ToBindOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new BindOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToBindOps0[TC[F[_]] <: Bind[F]] extends ToBindOpsU[TC] {
  implicit def ToBindOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new BindOps[F,A](v)

  ////

  ////
}

trait ToBindOps[TC[F[_]] <: Bind[F]] extends ToBindOps0[TC] with ToApplyOps[TC]

trait BindSyntax[F[_]] extends ApplySyntax[F] {
  implicit def ToBindOps[A](v: F[A]): BindOps[F, A] = new BindOps[F,A](v)(BindSyntax.this.F)

  def F: Bind[F]
  ////

  ////
}
