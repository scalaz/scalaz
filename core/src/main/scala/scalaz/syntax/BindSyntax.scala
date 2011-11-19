package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bind` */
trait BindV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Bind[F]
  ////
  import Liskov.<~<

  def flatMap[B](f: A => F[B]) = F.bind(self)(f)

  def >>=[B](f: A => F[B]) = F.bind(self)(f)

  def join[B](implicit ev: A <~< F[B]): F[B] = F.bind(self)(ev(_))

  def >>[B](b: F[B]): F[B] = F.bind(self)(_ => b)

  ////
}

trait ToBindV extends ToApplyV {
  implicit def ToBindV[FA](v: FA)(implicit F0: Unapply[Bind, FA]) =
    new BindV[F0.M,F0.A] { def self = F0(v); implicit def F: Bind[F0.M] = F0.TC }

  ////

  ////
}

trait BindSyntax[F[_]] extends ApplySyntax[F] {
  implicit def ToBindV[A](v: F[A])(implicit F0: Bind[F]): BindV[F, A] = new BindV[F,A] { def self = v; implicit def F: Bind[F] = F0 }

  ////

  ////
}
