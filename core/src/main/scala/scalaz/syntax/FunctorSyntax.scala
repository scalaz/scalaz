package scalaz
package syntax

trait FunctorV[F[_], A] extends SyntaxV[F[A]] {
  def self: F[A]

  def map[B](f: A => B)(implicit F: Functor[F]) = F.map(self)(f)

  def strengthL[B](b: B)(implicit F: Functor[F]) = F.strengthL(b, self)

  def strengthR[B](b: B)(implicit F: Functor[F]) = F.strengthR(self, b)
}


trait ToFunctorSyntax {
  implicit def functor[F[_], A](v: F[A]) = (new FunctorSyntax[F] {}).functorV(v)
  implicit def functorBin[F[_, _], X, A](v: F[X, A]) =
    (new FunctorSyntax[({type f[a] = F[X, a]})#f] {}).functorV(v)

  implicit def lift[F[_], A, B](v: A => B) = (new FunctorSyntax[F] {}).liftV(v)
}

trait FunctorSyntax[F[_]] {
  implicit def functorV[A](v: F[A]) = new FunctorV[F, A] {
    def self = v
  }

  implicit def liftV[A, B](v: A => B) = new LiftV[F, A, B] {
    def self = v
  }
}

trait LiftV[F[_],A,B] extends SyntaxV[A => B] {
  def lift(implicit F: Functor[F]) = F(self)
}


