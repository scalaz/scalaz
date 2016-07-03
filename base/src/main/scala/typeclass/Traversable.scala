package scalaz
package typeclass

trait Traversable[T[_]] {
  def functor: Functor[T]
  def foldable: Foldable[T]

  def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]]
  def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]]
}

object Traversable extends TraversableInstances {

  trait Sequence[T[_]] extends SequenceXorTraverse[Sequence[T]] { self : Traversable[T] =>
    override def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]] = traverse(ta)(identity)
  }
  trait Traverse[T[_]] extends SequenceXorTraverse[Traverse[T]] { self : Traversable[T] =>
    override def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]] = sequence(functor.map(ta)(f))
  }
  trait SequenceXorTraverse[D <: SequenceXorTraverse[D]]

  def apply[T[_]](implicit T: Traversable[T]): Traversable[T] = T

  object syntax extends TraversableSyntax
}
