package scalaz
package concurrent
package strategy

/**
 * A strategy that performs no evaluation of its argument.
 */
object Id {
  implicit def strategy[A]: Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = a
  }
}
