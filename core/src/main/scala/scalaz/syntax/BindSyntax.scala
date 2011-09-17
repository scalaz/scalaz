package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bind` */
trait BindV[F[_],A] extends SyntaxV[F[A]] {
  ////
  def flatMap[B](f: A => F[B])(implicit F: Bind[F]) = F.bind(self)(f)

  def >>=[B](f: A => F[B])(implicit F: Bind[F]) = F.bind(self)(f)

  def join[B](implicit F: Bind[F], ev: F[A] <:< F[F[B]]) = F.join(ev(self))
  ////
}

trait ToBindSyntax extends ToApplySyntax {
  implicit def ToBindV[F[_],A](v: F[A]) =
    (new BindSyntax[F] {}).ToBindV(v)
  implicit def ToBindVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new BindSyntax[({type f[a] = F[X, a]})#f] {}).ToBindV(v)
  implicit def ToBindVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new BindSyntax[({type f[a] = F[X, G, a]})#f] {}).ToBindV(v)
  implicit def ToBindVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new BindSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToBindV(v)

  ////

  ////
}

trait BindSyntax[F[_]] extends ApplySyntax[F] {
  implicit def ToBindV[A](v: F[A]): BindV[F, A] = new BindV[F,A] { def self = v }

  ////

  ////
}
