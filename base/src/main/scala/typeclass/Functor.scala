package scalaz
package typeclass

trait Functor[F[_]] {
  def map[A, B](ma: F[A])(f: A => B): F[B]
}

object Functor extends FunctorFunctions {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  object syntax extends FunctorSyntax
}
