package scalaz
package typeclass

trait Traversable[T[_]] {
  def functor: Functor[T]
  def foldable: Foldable[T]

  def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]]
  def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]]
}

object Traversable extends TraversableInstances with TraversableSyntax {
  def apply[T[_]](implicit T: Traversable[T]): Traversable[T] = T
}
