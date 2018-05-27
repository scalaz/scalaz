package scalaz
package types

/**
 * Witnesses that the type constructor `F[_]` is covariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
trait IsCovariantClass[F[_]] {
  def substCv[G[+ _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]]

  def substCt[G[- _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]]

  def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B]

  def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B]
}

object IsCovariantClass {

  /** Unsafe witness that the type constructor `F[_]` is covariant. */
  def unsafeForce[F[_]]: IsCovariant[F] =
    instanceOf(new IsCovariantClass.LiftLiskov[F] {
      override def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B] = {
        val _ = ev
        As.unsafeForce[F[A], F[B]]
      }
    })

  trait SubstCv[F[_]] extends IsCovariantClass[F] with Alt[SubstCv[F]] {
    final override def substCt[G[- _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
      type H[+a] = G[a] => G[F[A]]
      substCv[H, A, B](identity[G[F[A]]]).apply(g)
    }

    final override def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B] =
      substCv[F[A] <~< +?, A, B](As.refl[F[A]])

    final override def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
      substCv[F[A] => +?, A, B](identity[F[A]]).apply(fa)
  }

  trait SubstCt[F[_]] extends IsCovariantClass[F] with Alt[SubstCt[F]] {
    final override def substCv[G[+ _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
      type H[-a] = G[a] => G[F[B]]
      substCt[H, A, B](identity[G[F[B]]]).apply(g)
    }

    final override def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B] =
      substCt[-? <~< F[B], A, B](As.refl[F[B]])

    final override def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
      substCt[-? => F[B], A, B](identity[F[B]]).apply(fa)
  }

  trait LiftLiskov[F[_]] extends IsCovariantClass[F] with Alt[LiftLiskov[F]] {
    final override def substCv[G[+ _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
      liftLiskov[A, B].substCv[G](g)

    final override def substCt[G[- _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
      liftLiskov[A, B].substCt[G](g)

    final override def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
      liftLiskov[A, B].apply(fa)
  }

  trait Alt[D <: Alt[D]] { self: D =>
  }
}

trait IsCovariantInstances {
  implicit def scalaCovariant[F[+ _]]: IsCovariant[F] =
    instanceOf(new IsCovariantClass.SubstCv[F] {
      def substCv[G[+ _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
        type H[+T] = G[F[T]]
        ev.substCv[H](g)
      }
    })
}
