package scalaz.concurrent.strategy

import scalaz.concurrent.Strategy

/**
 * A strategy that evaluates its argument in the current thread.
 */
object Sequential {
  implicit def strategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = {
      val v = a()
      () => v
    }
  }
}
