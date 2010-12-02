package scalaz

/**
 * A categorical monoid.
 *
 * <p>
 * All monoid instances must satisfy the semigroup law and 2 additional laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a. append(zero, a) == a</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. append(a, zero) == a</code></li>
 * </p>
 */
trait Monoid[M] extends Zero[M] with Semigroup[M]

abstract class MonoidLow {
  implicit def monoid[M](implicit s: Semigroup[M], z: Zero[M]): Monoid[M] = new Monoid[M] {
    def append(s1: M, s2: => M) = s append (s1, s2)

    val zero = z.zero
  }
}

object Monoid extends MonoidLow {
  import Semigroup._
  import Zero._

  implicit def EitherLeftMonoid[A, B](implicit bz: Zero[B]) = monoid[Either.LeftProjection[A, B]](EitherLeftSemigroup, EitherLeftZero[A, B](bz))
}
