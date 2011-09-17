package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
trait ApplyV[F[_],A] extends SyntaxV[F[A]] {
  ////
  def <*>[B](f: F[A => B])(implicit F: Apply[F]) = F.ap(self)(f)
  ////
}

trait ToApplySyntax extends ToFunctorSyntax {
  implicit def ToApplyV[F[_],A](v: F[A]) =
    (new ApplySyntax[F] {}).ToApplyV(v)
  implicit def ToApplyVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new ApplySyntax[({type f[a] = F[X, a]})#f] {}).ToApplyV(v)
  implicit def ToApplyVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ApplySyntax[({type f[a] = F[X, G, a]})#f] {}).ToApplyV(v)
  implicit def ToApplyVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ApplySyntax[({type f[a] = F[X, Id, a]})#f] {}).ToApplyV(v)

  ////

  ////
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToApplyV[A](v: F[A]): ApplyV[F, A] = new ApplyV[F,A] { def self = v }

  ////

  ////
}
