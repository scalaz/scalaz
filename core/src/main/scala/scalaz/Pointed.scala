package scalaz

trait Pointed[F[_]] extends Functor[F] { self =>
  ////

  def pure[A](a: => A): F[A]

  ////
  val pointedSyntax = new scalaz.syntax.PointedSyntax[F] {}
}

object Pointed {
  def apply[F[_]](implicit F: Pointed[F]): Pointed[F] = F

  ////

  ////
}

