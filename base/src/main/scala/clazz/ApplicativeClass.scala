package scalaz
package clazz

trait ApplicativeClass[F[_]] extends Applicative[F] with ApplyClass[F] {
  implicit final def applicative: Applicative[F] = this
}

object ApplicativeClass {
  trait Template[F[_]] extends ApplicativeClass[F] with Map[F]

  trait Map[F[_]] extends Functor[F] { self: Applicative[F] =>
    override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = apply.ap(ma)(pure(f))
  }
}
