package scalaz
package clazz

trait Apply[F[_]] {
  def functor: Functor[F]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

object Apply {
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  object syntax extends ApplySyntax
}
