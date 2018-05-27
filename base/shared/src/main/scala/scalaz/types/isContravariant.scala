package scalaz
package types

/**
 * Witnesses that the type constructor `F[_]` is contravariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
trait IsContravariantClass[F[_]] {
  def substCv[G[+ _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]]

  def substCt[G[- _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]]

  def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A]

  def widen[A, B](fa: F[B])(implicit ev: A <~< B): F[A]
}

object IsContravariantClass {

  trait SubstCv[F[_]] extends IsContravariantClass[F] with Alt[SubstCv[F]] {
    final override def substCt[G[- _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
      type H[+a] = G[a] => G[F[B]]
      substCv[H, A, B](identity[G[F[B]]]).apply(g)
    }

    final override def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A] =
      substCv[F[B] <~< +?, A, B](As.refl[F[B]])

    final override def widen[A, B](fb: F[B])(implicit ev: A <~< B): F[A] =
      substCv[F[B] => +?, A, B](identity[F[B]]).apply(fb)
  }

  trait SubstCt[F[_]] extends IsContravariantClass[F] with Alt[SubstCt[F]] {
    final override def substCv[G[+ _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
      type H[-a] = G[a] => G[F[A]]
      substCt[H, A, B](identity[G[F[A]]]).apply(g)
    }

    final override def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A] =
      substCt[-? <~< F[A], A, B](As.refl[F[A]])

    final override def widen[A, B](fb: F[B])(implicit ev: A <~< B): F[A] =
      substCt[-? => F[A], A, B](identity[F[A]]).apply(fb)
  }

  trait LiftLiskov[F[_]] extends IsContravariantClass[F] with Alt[LiftLiskov[F]] {
    final override def substCv[G[+ _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
      liftLiskov[A, B].substCv[G](g)

    final override def substCt[G[- _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
      liftLiskov[A, B].substCt[G](g)

    final override def widen[A, B](fb: F[B])(implicit ev: A <~< B): F[A] =
      liftLiskov[A, B].apply(fb)
  }

  trait Alt[D <: Alt[D]]
}

trait IsContravariantInstances {
  implicit def scalaContravariant[F[- _]]: IsContravariant[F] =
    instanceOf(new IsContravariantClass.SubstCv[F] {
      def substCv[G[+ _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
        type H[-T] = G[F[T]]
        ev.substCt[H](g)
      }
    })
}
