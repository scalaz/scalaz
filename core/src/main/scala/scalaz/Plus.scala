package scalaz

trait PlusLike[F[_]] extends FunctorLike[F] { self =>
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
  self : FunctorInstance[F] =>

  implicit val plusParents: FunctorInstance[F] = this
}

object Plus {
  def apply[F[_]](implicit F: Plus[F]): Plus[F] = F

  ////

  ////
}

trait PlusInstance[F[_]] extends Plus[F] with FunctorInstance[F]
