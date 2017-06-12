package scalaz
package typeclass

trait Phantom[F[_]] {
  def pmap[A, B](ma: F[A]): F[B]
}

object Phantom extends PhantomFunctions with PhantomSyntax {
  def apply[F[_]](implicit F: Phantom[F]): Phantom[F] = F
}
