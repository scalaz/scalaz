package scalaz

trait MonadLike[F[_]] extends ApplicativeLike[F] with BindLike[F] { derived =>
  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => pure(f(a)))

  override val syntax = new scalaz.syntax.MonadSyntax[F] {}
}
trait Monad[F[_]] extends MonadLike[F]
trait MonadInstance[F[_]] extends Monad[F] with BindInstance[F] with PointedInstance[F] 
