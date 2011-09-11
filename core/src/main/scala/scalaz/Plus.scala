package scalaz

trait PlusLike[F[_]] extends FunctorLike[F] {
  def plus[A](a: F[A], b: => F[A]): F[A]
  def empty[A]: F[A]

//  override val syntax = new scalaz.syntax.PlusSyntax[F] {}
}
trait Plus[F[_]] extends PlusLike[F]
trait PlusInstance[F[_]] extends Plus[F] with FunctorInstance[F]
