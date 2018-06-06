package scalaz
package types

import com.github.ghik.silencer.silent

/**
 * Witnesses that the type constructor `F[_]` is covariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
trait IsCovariantClass[F[_]] {
  def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B]

  def substCv[G[+ _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
    liftLiskov(ev).substCv[G](g)

  def substCt[G[- _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
    liftLiskov(ev).substCt[G](g)

  def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
    liftLiskov(ev).apply(fa)
}

object IsCovariantClass {

  /** Unsafe witness that the type constructor `F[_]` is covariant. */
  def unsafeForce[F[_]]: IsCovariant[F] =
    instanceOf(new IsCovariantClass[F] {
      override def liftLiskov[A, B](implicit @silent ev: A <~< B): F[A] <~< F[B] =
        As.unsafeForce[F[A], F[B]]
    })

}

trait IsCovariantInstances {
  implicit def scalaCovariant[F[+ _]]: IsCovariant[F] =
    instanceOf(new IsCovariantClass[F] {
      def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B] = ev.liftCv
    })
}
