package scalaz.tc

import scala.Int

/**
 * Band is a semigroup with idempotency.
 *
 */
trait BandClass[A] extends SemigroupClass[A] {
  override def exponent(a: A, i: Int): A = {
    val _ = i // allocates, unfortunately - if we can remove this, that would be great
    a
  }
}
