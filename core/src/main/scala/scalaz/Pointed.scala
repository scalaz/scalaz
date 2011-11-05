package scalaz

trait Pointed[F[_]] extends Functor[F] { self =>
  ////

  def point[A](a: => A): F[A]

  // derived functions

  /** alias for `point` */
  def pure[A](a: => A): F[A] = point(a)

  ////
  val pointedSyntax = new scalaz.syntax.PointedSyntax[F] {}
}

object Pointed {
  def apply[F[_]](implicit F: Pointed[F]): Pointed[F] = F

  ////

  ////
}

