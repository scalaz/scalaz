package scalaz
package typeclass

trait Monad[M[_]] {
  def applicative: Applicative[M]
  def bind: Bind[M]
}

object Monad extends MonadInstances {
  trait Map[M[_]] extends Bind.FlatMap[M] { self: Monad[M] with Bind[M] with Applicative[M] with Functor[M] =>
    override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  def apply[M[_]](implicit M: Monad[M]): Monad[M] = M
}

