package scalaz

trait Plus[F[_]] extends Functor[F] with Empty[F] { self =>
  ////

  def plus[A](a: F[A], b: => F[A]): F[A]

  ////
  val plusSyntax = new scalaz.syntax.PlusSyntax[F] {}
}

object Plus {
  def apply[F[_]](implicit F: Plus[F]): Plus[F] = F

  ////

  ////
}

