package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
trait PlusV[F[_],A] extends SyntaxV[F[A]] {
  ////

  def <+>(other: => F[A])(implicit F: Plus[F]) = F.plus(self, other)

  ////
}

trait ToPlusSyntax extends ToFunctorSyntax with ToEmptySyntax {
  implicit def ToPlusV[F[_],A](v: F[A]) =
    new PlusV[F,A] { def self = v }
  implicit def ToPlusVFromBin[F[_, _], X, A](v: F[X, A]) =
    new PlusV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToPlusVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new PlusV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToPlusVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new PlusV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  ////
}

trait PlusSyntax[F[_]] extends FunctorSyntax[F] with EmptySyntax[F] {
  implicit def ToPlusV[A](v: F[A]): PlusV[F, A] = new PlusV[F,A] { def self = v }

  ////

  ////
}
