package scalaz

import std.option.{none, some}

/**
 * Inject type class as described in "Data types a la carte" (Swierstra 2008).
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf]]
 */
sealed abstract class Inject[F[_], G[_]] extends (F ~> G) {
  def apply[A](fa: F[A]): G[A] = inj(fa)
  def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
  def inj[A](fa: F[A]): G[A]
  def prj[A](ga: G[A]): Option[F[A]]
}

sealed abstract class InjectInstances {
  implicit def reflexiveInjectInstance[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      def inj[A](fa: F[A]) = fa
      def prj[A](ga: F[A]) = some(ga)
    }

  implicit def leftInjectInstance[F[_], G[_]]: Inject[F, Coproduct[F, G, *]]  =
    new Inject[F, Coproduct[F, G, *]] {
      def inj[A](fa: F[A]) = Coproduct.leftc(fa)
      def prj[A](ga: Coproduct[F, G, A]) = ga.run.fold(some, _ => none)
    }

  implicit def rightInjectInstance[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, *]] =
      new Inject[F, Coproduct[H, G, *]] {
        def inj[A](fa: F[A]) = Coproduct.rightc(I.inj(fa))
        def prj[A](ga: Coproduct[H, G, A]) = ga.run.fold(_ => none, I.prj)
      }
}

object Inject extends InjectInstances {
  /**
   * This should only be used by third party coproduct encodings. If `G <:
   * Coproduct` it is likely to break typeclass coherence. Caveat Emptor.
   */
  def instance[F[_], G[_]](
    inj_ : F ~> G,
    prj_ : G ~> λ[α => Option[F[α]]]
  ): Inject[F, G] = new Inject[F, G] {
    override def inj[A](fa: F[A]): G[A] = inj_(fa)
    override def prj[A](ga: G[A]): Option[F[A]] = prj_(ga)
  }

  def inject[F[_], G[_], A](ga: G[Free[F, A]])(implicit I: Inject[G, F]): Free[F, A] =
    Free[F, A](I.inj(ga))

  def match_[F[_], G[_], A](fa: Free[F, A])(implicit F: Functor[F], I: Inject[G, F]): Option[G[Free[F, A]]] =
    fa.resume.fold(I.prj, _ => none)

  @inline def apply[F[_], G[_]](implicit I: Inject[F, G]): Inject[F, G] = I
}
