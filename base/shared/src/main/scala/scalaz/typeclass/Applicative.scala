package scalaz
package typeclass

sealed trait Applicative[F[_]] extends Applicative.Class[F]

object Applicative {

  trait Class[F[_]] extends Apply.Class[F] {
    def pure[A](a: A): F[A]

    def applicative: Applicative[F]
  }

  trait Template[F[_]] extends Apply.Template[F] with Applicative[F]  {
    final override def applicative = this
  }

  trait DeriveMap[F[_]] extends Monad.Alt[DeriveMap[F]] { self: Class[F]  =>
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}