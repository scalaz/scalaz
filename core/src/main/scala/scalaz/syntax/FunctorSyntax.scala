package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Functor` */
trait FunctorV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Functor[F]
  ////
  final def map[B](f: A => B): F[B] = F.map(self)(f)
  final def ∘[B](f: A => B): F[B] = F.map(self)(f)
  final def strengthL[B](b: B): F[(B, A)] = F.strengthL(b, self)
  final def strengthR[B](b: B): F[(A, B)] = F.strengthR(self, b)
  ////
}

trait ToFunctorV  {
  implicit def ToFunctorV[F[_],A](v: F[A])(implicit F0: Functor[F]) =
    new FunctorV[F,A] { def self = v; implicit def F: Functor[F] = F0 }
  implicit def ToFunctorVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Functor[({type f[a] = F[X, a]})#f]) =
    new FunctorV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Functor[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToFunctorVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Functor[({type f[a] = F[X, G, a]})#f]) =
    new FunctorV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Functor[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToFunctorVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Functor[({type f[a] = F[X, Id, a]})#f]) =
    new FunctorV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Functor[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  implicit def ToLiftV[F[_], A, B](v: A => B) = new LiftV[F, A, B] { def self = v }

  // TODO Duplication
  trait LiftV[F[_], A, B] extends SyntaxV[A => B] {
    def lift(implicit F: Functor[F]) = F(self)
  }
  ////
}

trait FunctorSyntax[F[_]]  {
  implicit def ToFunctorV[A](v: F[A])(implicit F0: Functor[F]): FunctorV[F, A] = new FunctorV[F,A] { def self = v; implicit def F: Functor[F] = F0 }

  ////
  implicit def ToLiftV[A, B](v: A => B): LiftV[A, B] = new LiftV[A, B] {
    def self = v
  }

  trait LiftV[A,B] extends SyntaxV[A => B] {
    def lift(implicit F: Functor[F]) = F(self)
  }
  ////
}
