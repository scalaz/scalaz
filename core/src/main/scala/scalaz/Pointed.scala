package scalaz

trait PointedLike[F[_]] extends Functor[F] { self =>
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
trait Pointed[F[_]] extends PointedLike[F] {
  self  =>


}

object Pointed {
  def apply[F[_]](implicit F: Pointed[F]): Pointed[F] = F

  ////

  ////
}

trait PointedInstance[F[_]] extends Pointed[F] with FunctorInstance[F]
