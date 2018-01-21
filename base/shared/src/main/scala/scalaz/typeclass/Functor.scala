package scalaz
package typeclass

sealed trait Functor[F[_]] extends Functor.Class[F]

object Functor {
  trait Class[F[_]] {
    def map[A, B](ma: F[A])(f: A => B): F[B]

    def functor: Functor[F]
  }

  trait Template[F[_]] extends Functor[F] {
    final override def functor = this
  }

  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}