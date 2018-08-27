package scalaz.tc

import scala.Int

/**
 * Band is a semigroup with idempotency.
 *
 */
trait BandClass[A] extends SemigroupClass[A] {
  override def exponent(a: A, i: Int): A = {
    val _ = i
    a
  }
}
