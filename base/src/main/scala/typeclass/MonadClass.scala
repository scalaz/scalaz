package scalaz
package typeclass

import BindClass._

trait MonadClass[M[_]] extends Monad[M] with BindClass[M] with ApplicativeClass[M] {
  final def monad: Monad[M] = this
}

object MonadClass {
  trait Template[M[_]] extends MonadClass[M] with Map[M]

  trait Map[M[_]] extends FlatMap[M] { self: Monad[M] with Bind[M] with Applicative[M] with Functor[M] =>
    override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }
}
