package scalaz
package typeclass

trait Phantom[F[_]] {
  def pmap[A, B](ma: F[A]): F[B]
}
