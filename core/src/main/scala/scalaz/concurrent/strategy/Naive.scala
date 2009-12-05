package scalaz.concurrent.strategy

import scalaz.concurrent.Strategy

/**
 * A simple strategy that spawns a new thread for every evaluation.
 */
object Naive {
  import concurrent.ops._
  implicit def strategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = future {a()}
  }
}
