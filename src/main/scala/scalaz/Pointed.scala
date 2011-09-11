package scalaz

import syntax.PointedSyntax

trait PointedLike[F[_]] extends FunctorLike[F] { 
  def pure[A](a: => A): F[A]

  override val syntax = new PointedSyntax[F] {}
}
trait Pointed[F[_]] extends PointedLike[F]
trait PointedInstance[F[_]] extends FunctorInstance[F] with Pointed[F]
