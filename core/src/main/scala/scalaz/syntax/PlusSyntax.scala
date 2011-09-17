package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
trait PlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  def <+>(other: => F[A])(implicit F: Plus[F]) = F.plus(self, other)

  ////
}

trait ToPlusSyntax extends ToFunctorSyntax {
  implicit def plus[F[_],A](v: F[A]) =
    (new PlusSyntax[F] {}).plusV(v)
  implicit def plusBin[F[_, _], X, A](v: F[X, A]) =
    (new PlusSyntax[({type f[a] = F[X, a]})#f] {}).plusV(v)
  implicit def plusBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new PlusSyntax[({type f[a] = F[X, G, a]})#f] {}).plusV(v)
  implicit def plusBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new PlusSyntax[({type f[a] = F[X, Id, a]})#f] {}).plusV(v)

  ////

  ////
}

trait PlusSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def plusV[A](v: F[A]): PlusV[F, A] = new PlusV[F,A] { def self = v }

  ////

  ////
}
