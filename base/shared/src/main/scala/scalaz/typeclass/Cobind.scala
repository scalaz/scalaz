package scalaz
package typeclass

trait Cobind[F[_]] {
  def functor: Functor[F]
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]
  def cojoin[A](fa: F[A]): F[F[A]]
}
