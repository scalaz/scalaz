package scalaz
package typeclass

trait Functor[F[_]] {
  def map[A, B](ma: F[A])(f: A => B): F[B]
  def mapConst[A, B](ma: F[A])(c: B): F[B]
}

object Functor extends FunctorFunctions with FunctorSyntax {
  private[scalaz] trait MapConst
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}
