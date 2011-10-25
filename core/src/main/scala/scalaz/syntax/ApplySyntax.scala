package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
trait ApplyV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Apply[F]
  ////
  def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  ////
}

trait ToApplyV extends ToFunctorV {
  implicit def ToApplyV[F[_],A](v: F[A])(implicit F0: Apply[F]) =
    new ApplyV[F,A] { def self = v; implicit def F: Apply[F] = F0 }
  implicit def ToApplyVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Apply[({type f[a] = F[X, a]})#f]) =
    new ApplyV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Apply[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToApplyVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Apply[({type f[a] = F[X, G, a]})#f]) =
    new ApplyV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Apply[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToApplyVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Apply[({type f[a] = F[X, Id, a]})#f]) =
    new ApplyV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Apply[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToApplyV[A](v: F[A])(implicit F0: Apply[F]): ApplyV[F, A] = new ApplyV[F,A] { def self = v; implicit def F: Apply[F] = F0 }

  ////

  ////
}
