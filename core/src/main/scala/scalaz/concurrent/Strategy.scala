package scalaz
package concurrent

trait Strategy[A] {
  /**
   * Evaluate `a` in a manner specific to this strategy. A typical strategy will schedule asynchronous
   * evaluation of `a` and return a function that, when itself evaluated, will block until the result is ready.
   *
   * @return A function that returns the value of A.
   */
  def apply(a: () => A): () => A
}

object Strategy {
  implicit def StrategyFrom[A](f: (() => A) => () => A): Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = f(a)
  }

  implicit def StrategyTo[A](s: Strategy[A]): (() => A) => () => A = (a: () => A) => s(a)
}
