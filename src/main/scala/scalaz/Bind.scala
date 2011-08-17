package scalaz

trait BindLike[F[_]] extends ApplyLike[F] { self => 
  def bind[A,B](fa: F[A])(f: A => F[B]): F[B] 

  def ap[A,B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(f => map(fa)(f))
  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)
}
trait Bind[F[_]] extends BindLike[F]
trait BindInstance[F[_]] extends Bind[F] with ApplicativeInstance[F]

trait BindV[F[_],A] extends SyntaxV[F[A]] {
  def flatMap[B](f: A => F[B])(implicit F: Bind[F]) = F.bind(self)(f)
}
trait JoinV[F[_],A] extends SyntaxV[F[F[A]]] {
  def join(implicit F: Bind[F]) = F.join(self)
}

trait ToBindSyntax extends ToApplySyntax { 
  implicit def bind[F[_],A](v: F[A]) = (new BindSyntax[F] {}).bindV(v)
}
trait BindSyntax[F[_]] extends ApplySyntax[F] { 
  implicit def bindV[A](v: F[A]) = new BindV[F,A] { def self = v }
}
