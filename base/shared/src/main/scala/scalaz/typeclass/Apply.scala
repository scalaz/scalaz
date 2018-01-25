package scalaz
package typeclass

sealed trait Apply[F[_]] extends Apply.Class[F]

object Apply {

  trait Class[F[_]] extends Functor.Class[F] {
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

    def apply: Apply[F]
  }

  trait Template[F[_]] extends Functor.Template[F] with Apply[F] {
    final override def apply = this
  }

  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F
}