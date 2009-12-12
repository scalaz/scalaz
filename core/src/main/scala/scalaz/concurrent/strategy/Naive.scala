package scalaz
package concurrent
package strategy

/**
 * A simple strategy that spawns a new thread for every evaluation.
 */
object Naive {
  import scala.concurrent.ops.future
  implicit def strategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = future {a()}
  }
}
