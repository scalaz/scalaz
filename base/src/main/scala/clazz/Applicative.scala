package scalaz
package clazz

trait Applicative[F[_]] {
  def apply: Apply[F]
  def pure[A](a: A): F[A]
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  object syntax extends ApplicativeSyntax
}
