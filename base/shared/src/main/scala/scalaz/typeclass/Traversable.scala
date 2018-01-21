package scalaz
package typeclass

sealed trait Traversable[T[_]] extends Traversable.Class[T]

object Traversable {

  trait Class[T[_]] extends Functor.Class[T] with Foldable.Class[T] {
    def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]]
    def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]]

    def traversable: Traversable[T]
  }

  trait Template[T[_]] extends Functor.Template[T] with Foldable.Template[T] with Traversable[T] {
    final override def traversable = this
  }

  trait Alt[D <: Alt[D]]
  trait DeriveSequence[T[_]] extends Alt[DeriveSequence[T]] { self : Class[T] =>
    final override def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]] = traverse(ta)(identity)
  }
  trait DeriveTraverse[T[_]] extends Alt[DeriveTraverse[T]] { self : Class[T] =>
    final override def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] = sequence(map(ta)(f))
  }

  def apply[T[_]](implicit T: Traversable[T]): Traversable[T] = T
}
