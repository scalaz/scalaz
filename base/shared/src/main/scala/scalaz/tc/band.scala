package scalaz.tc

import scala.annotation.tailrec

/**
  * Band is a semigroup with idempotency.
  *
  */
trait BandClass[A] extends SemigroupClass[A] {

  /**
    * For `n = 0`, `value`
    * For `n = 1`, `append(value, value)`
    * For `n = 2`, `append(append(value, value), value)`
    *
    * The default definition uses peasant multiplication, exploiting associativity to only
    * require `O(log n)` uses of [[mappend]]
    */
  def multiply1(value: A, n: Int): A = {
    @tailrec
    def go(x: A, y: Int, z: A): A = y match {
      case y if (y & 1) == 0 => go(mappend(x, x), y >>> 1, z)
      case y if (y == 1)     => mappend(x, z)
      case _                 => go(mappend(x, x), (y - 1) >>>  1, mappend(x, z))
    }
    if (n <= 0) value else go(value, n, value)
  }
}
