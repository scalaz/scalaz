package scalaz
package types

import scala.AnyVal

/**
 * The denial inequality is a symmetric irreflexive binary relation with
 * the additional condition that if two elements are apart, then any other
 * element is apart from at least one of them (this last property is often
 * called co-transitivity or comparison). Co-transitivity for denial
 * inequality is *not* constructive.
 *
 * @see [[https://en.wikipedia.org/wiki/Apartness_relation
 *        Apartness relation]]
 */
final case class NotIs[A, B](run: (A === B) => Void) { self =>
  import NotIs._

  /**
   * Having `A === B` and `A =!= B` at the same time leads to a contradiction.
   */
  def apply(ab: A === B): Void = run(ab)

  /**
   * If `F[A] =!= F[B]`, then `A =!= B`. This is a contrapositive to
   * `(A === B) => (F[A] === F[B])`.
   */
  def lower[F[_]]: PartialLower[F, A, B] =
    new PartialLower[F, A, B](this)

  /**
   * Inequality is symmetric relation and therefore can be flipped around.
   * Flipping is its own inverse, so `x.flip.flip == x`.
   */
  def flip: B =!= A = NotIs.witness[B, A](ba => self(ba.flip))
}
object NotIs {
  def apply[A, B](implicit ev: NotIs[A, B]): NotIs[A, B] = ev

  /**
   * Inequality is an irreflexive relation.
   */
  def irreflexive[A](ev: A NotIs A): Void =
    ev.apply(Is.refl[A])

  def witness[A, B](f: (A === B) => Void): NotIs[A, B] =
    new NotIs[A, B](f)

  private[NotIs] final class PartialLower[F[_], A, B](val ab: NotIs[A, B]) extends AnyVal {
    def apply[X, Y](implicit A: A === F[X], B: B === F[Y]): X =!= Y =
      NotIs(xy => ab(A andThen xy.lift[F] andThen B.flip))
  }
}
