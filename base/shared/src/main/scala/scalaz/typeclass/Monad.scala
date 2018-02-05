package scalaz
package typeclass

trait Monad[M[_]] {
  def applicative: Applicative[M]
  def bind: Bind[M]
}

object Monad {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}
