package scalaz
package typeclass

trait Traversable[T[_]] {
  def functor: Functor[T] with this.type
  def foldable: Foldable[T] with this.type

  def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]]
  def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]]
}

