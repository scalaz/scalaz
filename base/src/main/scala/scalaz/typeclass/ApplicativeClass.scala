package scalaz
package typeclass

trait ApplicativeClass[F[_]] extends Applicative[F] with ApplyClass[F] {
  final def applicative: Applicative[F] = this
}

object ApplicativeClass {
  trait Template[F[_]] extends ApplicativeClass[F] with Map[F]

  trait Map[F[_]] { self: Applicative[F] with Apply[F] with Functor[F] =>
    override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }
}
