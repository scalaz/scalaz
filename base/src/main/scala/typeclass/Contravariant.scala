package scalaz
package typeclass

trait Contravariant[F[_]] {
  def contramap[A, B](r: F[A])(f: B => A): F[B]
}
