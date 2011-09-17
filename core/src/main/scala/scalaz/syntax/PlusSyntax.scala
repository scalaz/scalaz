package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
trait PlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  def <+>(other: => F[A])(implicit F: Plus[F]) = F.plus(self, other)

  ////
}

trait ToPlusSyntax extends ToFunctorSyntax {
  implicit def ToPlusV[F[_],A](v: F[A]) =
    (new PlusSyntax[F] {}).ToPlusV(v)
  implicit def ToPlusVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new PlusSyntax[({type f[a] = F[X, a]})#f] {}).ToPlusV(v)
  implicit def ToPlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new PlusSyntax[({type f[a] = F[X, G, a]})#f] {}).ToPlusV(v)
  implicit def ToPlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new PlusSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToPlusV(v)

  ////

  ////
}

trait PlusSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToPlusV[A](v: F[A]): PlusV[F, A] = new PlusV[F,A] { def self = v }

  ////

  ////
}
