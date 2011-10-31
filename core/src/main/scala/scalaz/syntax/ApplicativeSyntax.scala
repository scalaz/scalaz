package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Applicative` */
trait ApplicativeV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Applicative[F]
  ////
  final def map2[B,C](fb: F[B])(f: (A,B) => C): F[C] = F.map2(self,fb)(f)
  final def pair[B](fb: F[B]): F[(A, B)] = F.map2(self, fb)((_,_))
  ////
}

trait ToApplicativeV extends ToApplyV with ToPointedV {
  implicit def ToApplicativeV[F[_],A](v: F[A])(implicit F0: Applicative[F]) =
    new ApplicativeV[F,A] { def self = v; implicit def F: Applicative[F] = F0 }
  implicit def ToApplicativeVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Applicative[({type f[a] = F[X, a]})#f]) =
    new ApplicativeV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Applicative[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToApplicativeVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Applicative[({type f[a] = F[X, G, a]})#f]) =
    new ApplicativeV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Applicative[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToApplicativeVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Applicative[({type f[a] = F[X, Id, a]})#f]) =
    new ApplicativeV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Applicative[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] with PointedSyntax[F] {
  implicit def ToApplicativeV[A](v: F[A])(implicit F0: Applicative[F]): ApplicativeV[F, A] = new ApplicativeV[F,A] { def self = v; implicit def F: Applicative[F] = F0 }

  ////
  implicit def lift2[A,B,C](f: (A,B) => C)(implicit F: Applicative[F]) = F.lift2(f)
  implicit def lift3[A,B,C,D](f: (A,B,C) => D)(implicit F: Applicative[F]) = F.lift3(f)
  ////
}
