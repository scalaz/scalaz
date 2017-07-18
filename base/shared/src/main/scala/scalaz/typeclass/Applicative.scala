package scalaz
package typeclass

trait Applicative[F[_]] {
  def apply: Apply[F]
  def pure[A](a: A): F[A]
}

object Applicative extends ApplicativeSyntax {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}
