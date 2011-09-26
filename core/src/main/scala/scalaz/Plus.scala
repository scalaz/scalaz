package scalaz

trait PlusLike[F[_]] extends Functor[F] { self =>
  ////

  def plus[A](a: F[A], b: => F[A]): F[A]
  def empty[A]: F[A]

  ////
  val plusSyntax = new scalaz.syntax.PlusSyntax[F] {}
}

////
/**
 *
 */
////
trait Plus[F[_]] extends PlusLike[F] {
  self  =>


}

object Plus {
  def apply[F[_]](implicit F: Plus[F]): Plus[F] = F

  ////

  ////
}

trait PlusInstance[F[_]] extends Plus[F] with FunctorInstance[F]
