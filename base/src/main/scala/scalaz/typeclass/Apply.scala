package scalaz
package typeclass

trait Apply[F[_]] {
  def functor: Functor[F]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

object Apply extends ApplySyntax {
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F
}
