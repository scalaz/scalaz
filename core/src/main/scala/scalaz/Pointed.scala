package scalaz

trait PointedLike[F[_]] extends FunctorLike[F] { self =>
  ////

  def pure[A](a: => A): F[A]

  ////
  val pointedSyntax = new scalaz.syntax.PointedSyntax[F] {}
}

////
/**
 *
 */
////
trait Pointed[F[_]] extends PointedLike[F]

trait PointedInstance[F[_]] extends Pointed[F] with FunctorInstance[F]
