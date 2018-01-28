package scalaz
package typeclass

trait Comonad[F[_]] {
  def cobind: Cobind[F] 
  def copoint[A](fa: F[A]): A
}

object Comonad {
  def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F
}
