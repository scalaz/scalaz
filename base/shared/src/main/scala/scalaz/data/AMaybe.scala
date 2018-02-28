package scalaz
package data

/** Similar to `Option[F[A, B]]`, except that
 * the empty case witnesses type equality between `A` and `B`.
 */
sealed abstract class AMaybe[F[_, _], A, B]

final case class AJust[F[_, _], A, B](value: F[A, B]) extends AMaybe[F, A, B]

/**
 * The empty case contains evidence of type equality
 * to overcome the limitations of pattern-matching on GADTs.
 */
sealed abstract case class AEmpty[F[_, _], A, B]() extends AMaybe[F, A, B] {
  def subst[G[_]](ga: G[A]): G[B]
  def unsubst[G[_]](gb: G[B]): G[A]
  def leibniz: A Is B = subst[A === ?](Is.refl)
}

object AMaybe extends AMaybeInstances {
  def empty[F[_, _], A]: AMaybe[F, A, A] = None.asInstanceOf[AMaybe[F, A, A]]

  private val None = none[Nothing, Nothing]
  private def none[F[_, _], A]: AMaybe[F, A, A] = new AEmpty[F, A, A] {
    def subst[G[_]](ga: G[A]): G[A]   = ga
    def unsubst[G[_]](gb: G[A]): G[A] = gb
  }
}
