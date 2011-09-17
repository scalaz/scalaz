package scalaz

trait MonadLike[F[_]] extends ApplicativeLike[F] with BindLike[F] { self =>
  ////

  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => pure(f(a)))

  ////
  val monadSyntax = new scalaz.syntax.MonadSyntax[F] {}
}

////
/**
 *
 */
////
trait Monad[F[_]] extends MonadLike[F]

object Monad {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  ////

  ////
}

trait MonadInstance[F[_]] extends Monad[F] with ApplicativeInstance[F] with BindInstance[F]
