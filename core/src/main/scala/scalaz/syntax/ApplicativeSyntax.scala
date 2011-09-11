package scalaz
package syntax

trait ApplicativeV[F[_],A] extends SyntaxV[F[A]] {
  def map2[B,C](fb: F[B])(f: (A,B) => C)(implicit F: Applicative[F]) = F.map2(self,fb)(f)
  def pair[B](fb: F[B])(implicit F: Applicative[F]) = F.map2(self, fb)((_,_))
}

trait ToApplicativeSyntax extends ToApplySyntax with ToPointedSyntax {
  implicit def applicative[F[_],A](v: F[A]) = (new ApplicativeSyntax[F] {}).applicativeV(v)
}
trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] with PointedSyntax[F] {
  implicit def applicativeV[A](v: F[A]) = new ApplicativeV[F,A] { def self = v }
  implicit def lift2V[A,B,C](f: (A,B) => C)(implicit F: Applicative[F]) = F.lift2(f)
  implicit def lift3V[A,B,C,D](f: (A,B,C) => D)(implicit F: Applicative[F]) = F.lift3(f)
}
