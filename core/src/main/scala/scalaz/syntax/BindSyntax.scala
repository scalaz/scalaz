package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bind` */
trait BindV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Bind[F]
  ////
  def flatMap[B](f: A => F[B]) = F.bind(self)(f)

  def >>=[B](f: A => F[B]) = F.bind(self)(f)

  def join[B](implicit ev: F[A] <:< F[F[B]]) = F.join(ev(self))
  ////
}

trait ToBindSyntax extends ToApplySyntax {
  implicit def ToBindV[F[_],A](v: F[A])(implicit F0: Bind[F]) =
    new BindV[F,A] { def self = v; implicit def F: Bind[F] = F0 }
  implicit def ToBindVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Bind[({type f[a] = F[X, a]})#f]) =
    new BindV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Bind[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToBindVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Bind[({type f[a] = F[X, G, a]})#f]) =
    new BindV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Bind[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToBindVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Bind[({type f[a] = F[X, Id, a]})#f]) =
    new BindV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Bind[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait BindSyntax[F[_]] extends ApplySyntax[F] {
  implicit def ToBindV[A](v: F[A])(implicit F0: Bind[F]): BindV[F, A] = new BindV[F,A] { def self = v; implicit def F: Bind[F] = F0 }

  ////

  ////
}
