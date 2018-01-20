package scalaz
package typeclass

trait Comonad[F[_]] {
  def cobind: Cobind[F] with this.type
  def copoint[A](fa: F[A]): A
}

object Comonad extends ComonadSyntax {
  def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F
}
