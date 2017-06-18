package scalaz
package typeclass

trait TraversableClass[T[_]] extends Traversable[T] with FunctorClass[T] with FoldableClass[T] {
  final def traversable: Traversable[T] = this
}

object TraversableClass {
  trait Traverse[T[_]] extends Alt[Traverse[T]] { self : Traversable[T] =>
    override def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]]
    override def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]] = traverse(ta)(identity)
  }

  trait Sequence[T[_]] extends Alt[Sequence[T]] { self : Traversable[T] =>
    override def sequence[F[_], A](ta: T[F[A]])(implicit F: Applicative[F]): F[T[A]]
    override def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: Applicative[F]): F[T[B]] = sequence(functor.map(ta)(f))
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
