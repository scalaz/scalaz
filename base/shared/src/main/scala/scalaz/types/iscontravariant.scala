package scalaz
package types

import com.github.ghik.silencer.silent

/**
 * Witnesses that the type constructor `F[_]` is contravariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
trait IsContravariantClass[F[_]] {
  def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A]

  def substCv[G[+ _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
    liftLiskov(ev).substCv(g)

  def substCt[G[- _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
    liftLiskov(ev).substCt(g)

  def widen[A, B](fa: F[B])(implicit ev: A <~< B): F[A] =
    liftLiskov(ev).apply(fa)
}

object IsContravariantClass {

  /** Unsafe witness that the type constructor `F[_]` is contravariant. */
  def unsafeForce[F[_]]: IsContravariant[F] =
    instanceOf(new IsContravariantClass[F] {
      override def liftLiskov[A, B](implicit @silent ev: A <~< B): F[B] <~< F[A] =
        As.unsafeForce[F[B], F[A]]
    })

}

trait IsContravariantInstances {
  implicit def scalaContravariant[F[- _]]: IsContravariant[F] =
    instanceOf(new IsContravariantClass[F] {
      def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A] = ev.liftCt
    })
}
