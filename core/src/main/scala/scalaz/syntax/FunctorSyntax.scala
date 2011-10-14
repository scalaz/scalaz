package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Functor` */
trait FunctorV[F[_],A] extends SyntaxV[F[A]] {
  ////

  def map[B](f: A => B)(implicit F: Functor[F]) = F.map(self)(f)

  def strengthL[B](b: B)(implicit F: Functor[F]) = F.strengthL(b, self)

  def strengthR[B](b: B)(implicit F: Functor[F]) = F.strengthR(self, b)

  ////
}

trait ToFunctorSyntax  {
  implicit def ToFunctorV[F[_],A](v: F[A]) =
    new FunctorV[F,A] { def self = v }
  implicit def ToFunctorVFromBin[F[_, _], X, A](v: F[X, A]) =
    new FunctorV[({type f[a] = F[X, a]})#f,A] { def self = v }
  implicit def ToFunctorVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    new FunctorV[({type f[a] = F[X, G, a]})#f,A] { def self = v }
  implicit def ToFunctorVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    new FunctorV[({type f[a] = F[X, Id, a]})#f,A] { def self = v }

  ////

  implicit def ToLiftV[F[_], A, B](v: A => B) = new LiftV[F, A, B] { def self = v }

  // TODO Duplication
  trait LiftV[F[_], A, B] extends SyntaxV[A => B] {
    def lift(implicit F: Functor[F]) = F(self)
  }
  ////
}

trait FunctorSyntax[F[_]]  {
  implicit def ToFunctorV[A](v: F[A]): FunctorV[F, A] = new FunctorV[F,A] { def self = v }

  ////
  implicit def ToLiftV[A, B](v: A => B): LiftV[A, B] = new LiftV[A, B] {
    def self = v
  }

  trait LiftV[A,B] extends SyntaxV[A => B] {
    def lift(implicit F: Functor[F]) = F(self)
  }
  ////
}
