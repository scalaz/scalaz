package scalaz

trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  ////

  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => pure(f(a)))

  ////
  val monadSyntax = new scalaz.syntax.MonadSyntax[F] {}
}

object Monad {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  ////

  ////
}

