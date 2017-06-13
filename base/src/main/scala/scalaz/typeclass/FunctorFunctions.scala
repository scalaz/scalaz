package scalaz
package typeclass

trait FunctorFunctions {
  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)
}
