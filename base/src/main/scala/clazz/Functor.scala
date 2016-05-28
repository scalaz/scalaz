package scalaz
package clazz

trait Functor[F[_]] {
  def map[A, B](ma: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  object syntax extends FunctorSyntax
}
