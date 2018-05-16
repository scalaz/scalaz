package scalaz
package ct

trait ApplyClass[F[_]] extends FunctorClass[F] {
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}
