package scalaz
package clazz

trait MonadClass[F[_]] extends Monad[F] with BindClass[F] with ApplicativeClass[F] {
  implicit final def monad: Monad[F] = this
}

object MonadClass {
  trait Template[F[_]] extends MonadClass[F] with Map[F] with BindClass.Ap[F]

  trait Map[F[_]] extends Functor[F] { self: Monad[F] =>
    override final def map[A, B](ma: F[A])(f: (A) => B): F[B] = bind.flatMap(ma)(a => applicative.pure(f(a)))
  }
}
