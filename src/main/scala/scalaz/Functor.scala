package scalaz

trait FunctorLike[F[_]] { self =>
  def map[A,B](fa: F[A])(f: A => B): F[B]

  // derived functions
  def apply[A,B](f: A => B): F[A] => F[B] = map(_)(f)
  def strengthL[A,B](a: A, f: F[B]): F[(A,B)] = map(f)(b => (a,b))
  def strengthR[A,B](f: F[A], b: B): F[(A,B)] = map(f)(a => (a,b))
  lazy val Functor: Functor[F] = new Functor[F] { 
    def map[A,B](fa: F[A])(f: A => B) = FunctorLike.this.map(fa)(f) }
  val syntax = new FunctorSyntax[F] {}
}
trait Functor[F[_]] extends FunctorLike[F]
trait FunctorInstance[F[_]] extends Functor[F]

trait FunctorV[F[_],A] extends SyntaxV[F[A]] {
  def self: F[A]
  def map[B](f: A => B)(implicit F: Functor[F]) = F.map(self)(f)
  def strengthL[B](b: B)(implicit F: Functor[F]) = F.strengthL(b,self)
  def strengthR[B](b: B)(implicit F: Functor[F]) = F.strengthR(self,b)
}

trait LiftV[F[_],A,B] extends SyntaxV[A => B] {
  def lift(implicit F: Functor[F]) = F(self)
}

trait ToFunctorSyntax { 
  implicit def functor[F[_],A](v: F[A]) = (new FunctorSyntax[F] {}).functorV(v)
  implicit def lift[F[_],A,B](v: A => B) = (new FunctorSyntax[F] {}).liftV(v)
}
trait FunctorSyntax[F[_]] { 
  implicit def functorV[A](v: F[A]) = new FunctorV[F,A] { def self = v }
  implicit def liftV[A,B](v: A => B) = new LiftV[F,A,B] { def self = v }
}
