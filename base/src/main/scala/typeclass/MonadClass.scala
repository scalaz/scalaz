package scalaz
package typeclass

trait MonadClass[F[_]] extends Monad[F] with BindClass[F] with ApplicativeClass[F] {
  final def monad: Monad[F] = this
}

object MonadClass {
  trait Template[F[_]] extends MonadClass[F] with Map[F]

  trait Map[F[_]] { self: MonadClass[F] =>
    override def map[A, B](ma: F[A])(f: (A) => B): F[B] = flatMap(ma)(a => pure(f(a)))
  }
}
