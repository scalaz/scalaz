package scalaz

trait Bind[F[_]] extends Apply[F] { self =>
  ////

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(f => map(fa)(f))

  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)

  ////
  val bindSyntax = new scalaz.syntax.BindSyntax[F] {}
}

object Bind {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F

  ////

  ////
}

