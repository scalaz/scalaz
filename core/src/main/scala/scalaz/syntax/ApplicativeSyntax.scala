package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Applicative` */
trait ApplicativeV[F[_],A] extends SyntaxV[F[A]] {
  ////
  def map2[B,C](fb: F[B])(f: (A,B) => C)(implicit F: Applicative[F]) = F.map2(self,fb)(f)
  def pair[B](fb: F[B])(implicit F: Applicative[F]) = F.map2(self, fb)((_,_))
  ////
}

trait ToApplicativeSyntax extends ToApplySyntax with ToPointedSyntax {
  implicit def ToApplicativeV[F[_],A](v: F[A]) =
    (new ApplicativeSyntax[F] {}).ToApplicativeV(v)
  implicit def ToApplicativeVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new ApplicativeSyntax[({type f[a] = F[X, a]})#f] {}).ToApplicativeV(v)
  implicit def ToApplicativeVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new ApplicativeSyntax[({type f[a] = F[X, G, a]})#f] {}).ToApplicativeV(v)
  implicit def ToApplicativeVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new ApplicativeSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToApplicativeV(v)

  ////

  ////
}

trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] with PointedSyntax[F] {
  implicit def ToApplicativeV[A](v: F[A]): ApplicativeV[F, A] = new ApplicativeV[F,A] { def self = v }

  ////
  implicit def lift2[A,B,C](f: (A,B) => C)(implicit F: Applicative[F]) = F.lift2(f)
  implicit def lift3[A,B,C,D](f: (A,B,C) => D)(implicit F: Applicative[F]) = F.lift3(f)
  ////
}
