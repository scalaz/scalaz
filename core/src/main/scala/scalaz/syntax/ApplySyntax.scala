package scalaz
package syntax


trait ApplyV[F[_], A] extends SyntaxV[F[A]] {
  def <*>[B](f: F[A => B])(implicit F: Apply[F]) = F.ap(self)(f)
}

trait ToApplySyntax extends ToFunctorSyntax {
  implicit def apply[F[_], A](v: F[A]) = (new ApplySyntax[F] {}).applyV(v)

  implicit def applyBin[F[_, _], X, A](v: F[X, A]) = (new ApplySyntax[({type f[a] = F[X, a]})#f] {}).applyV(v)

  implicit def applyBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) = (new ApplySyntax[({type f[a] = F[X, G, a]})#f] {}).applyV(v)
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def applyV[A](v: F[A]) = new ApplyV[F, A] {
    def self = v
  }
}
