package scalaz

trait FunctorLike[F[_]] { self =>
  def map[A,B](fa: F[A])(f: A => B): F[B]

  // derived functions
  def apply[A,B](f: A => B): F[A] => F[B] = map(_)(f)
  def strengthL[A,B](a: A, f: F[B]): F[(A,B)] = map(f)(b => (a,b))
  def strengthR[A,B](f: F[A], b: B): F[(A,B)] = map(f)(a => (a,b))

  val syntax = new scalaz.syntax.FunctorSyntax[F] {}
}

trait Functor[F[_]] extends FunctorLike[F]
trait FunctorInstance[F[_]] extends Functor[F]
