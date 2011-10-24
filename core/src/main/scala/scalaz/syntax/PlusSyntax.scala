package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
trait PlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Plus[F]
  ////

  def <+>(other: => F[A]) = F.plus(self, other)

  ////
}

trait ToPlusSyntax extends ToFunctorSyntax with ToEmptySyntax {
  implicit def ToPlusV[F[_],A](v: F[A])(implicit F0: Plus[F]) =
    new PlusV[F,A] { def self = v; implicit def F: Plus[F] = F0 }
  implicit def ToPlusVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Plus[({type f[a] = F[X, a]})#f]) =
    new PlusV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Plus[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToPlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Plus[({type f[a] = F[X, G, a]})#f]) =
    new PlusV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Plus[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToPlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Plus[({type f[a] = F[X, Id, a]})#f]) =
    new PlusV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Plus[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait PlusSyntax[F[_]] extends FunctorSyntax[F] with EmptySyntax[F] {
  implicit def ToPlusV[A](v: F[A])(implicit F0: Plus[F]): PlusV[F, A] = new PlusV[F,A] { def self = v; implicit def F: Plus[F] = F0 }

  ////

  ////
}
