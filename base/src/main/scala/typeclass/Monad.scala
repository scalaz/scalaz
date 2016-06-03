package scalaz
package typeclass

trait Monad[M[_]] {
  def applicative: Applicative[M]
  def bind: Bind[M]
}

object Monad extends MonadInstances {
  def apply[M[_]](implicit M: Monad[M]): Monad[M] = M
}

