package scalaz

trait BindLike[F[_]] extends ApplyLike[F] { self =>
  def bind[A,B](fa: F[A])(f: A => F[B]): F[B]

  def ap[A,B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(f => map(fa)(f))
  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)
}
trait Bind[F[_]] extends BindLike[F]
trait BindInstance[F[_]] extends Bind[F] with ApplicativeInstance[F]
